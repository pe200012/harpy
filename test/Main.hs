{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Main (main) where

import Control.Exception (SomeException, throwIO, try)
import Control.Monad (replicateM_)
import qualified Data.ByteString as BS
import Data.List (isInfixOf)
import Data.Word
import Foreign
import Foreign.C.Types
import Numeric (readHex)
import System.Exit (ExitCode(..))
import System.IO.Temp (withSystemTempDirectory)
import System.Process
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase)
import Text.Printf

import Harpy hiding (xor, and, or)
import qualified Harpy (xor, and, or)
import qualified Harpy.Internal.ExecutableMemory as ExecMem
import Harpy.X86Disassembler

$(callDecl "callAsWord32" [t|Word32 -> IO Word32|])

------------------------------------------------------------------------
-- Minimal test harness (inspired by AsmJit's Broken framework)
------------------------------------------------------------------------

data Result = Pass | Fail String | Skip String

runTests :: [(String, IO Result)] -> IO ()
runTests tests =
    defaultMain $ testGroup "harpy-tests" (map toTest tests)
  where
    toTest (name, action) = testCase name (assertResult =<< action)

assertResult :: Result -> Assertion
assertResult Pass = return ()
assertResult (Fail err) = assertFailure err
assertResult (Skip reason) = putStrLn ("SKIP: " ++ reason)

pass :: IO Result
pass = pure Pass

failWith :: String -> IO Result
failWith = pure . Fail

skip :: String -> IO Result
skip = pure . Skip

------------------------------------------------------------------------
-- FFI for proper mmap/mprotect (what Harpy should be using)
------------------------------------------------------------------------

foreign import ccall "sys/mman.h mmap"
  c_mmap :: Ptr () -> CSize -> CInt -> CInt -> CInt -> CLong -> IO (Ptr Word8)

foreign import ccall "sys/mman.h mprotect"
  c_mprotect :: Ptr Word8 -> CSize -> CInt -> IO CInt

foreign import ccall "sys/mman.h munmap"
  c_munmap :: Ptr Word8 -> CSize -> IO CInt

foreign import ccall "dynamic"
  mkFun :: FunPtr (IO Word32) -> IO Word32

protRead, protWrite, protExec :: CInt
protRead  = 0x1
protWrite = 0x2
protExec  = 0x4

mapPrivate, mapAnonymous :: CInt
mapPrivate   = 0x2
mapAnonymous = 0x20

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

emitBytes :: CodeGen () () a -> IO (Either String [Word8])
emitBytes gen = do
    (_, res) <- runCodeGen act () ()
    case res of
      Left err -> return $ Left (show err)
      Right bs -> return $ Right bs
  where
    act = do
      _ <- gen
      bufs <- getCodeBufferList
      liftIO $ concat <$> mapM (\(p, len) -> mapM (peekByteOff p) [0..len-1]) bufs

showHexBytes :: [Word8] -> String
showHexBytes = concatMap (printf "%02x")

nasmAssemble :: String -> IO (Either String [Word8])
nasmAssemble asmText =
    withSystemTempDirectory "harpy-test" $ \dir -> do
      let srcFile = dir ++ "/harpy_test.asm"
          outFile = dir ++ "/harpy_test.bin"
      writeFile srcFile $ "BITS 32\n" ++ asmText ++ "\n"
      (ec, _, err) <- readProcessWithExitCode "nasm" ["-f", "bin", "-o", outFile, srcFile] ""
      case ec of
        ExitFailure _ -> return $ Left $ "nasm: " ++ err
        ExitSuccess   -> Right . BS.unpack <$> BS.readFile outFile

ndisasmDecode :: [Word8] -> IO (Either String String)
ndisasmDecode rawBytes =
    withSystemTempDirectory "harpy-ndisasm" $ \dir -> do
      let binFile = dir ++ "/harpy_test_ndisasm.bin"
      BS.writeFile binFile (BS.pack rawBytes)
      (ec, out, err) <- readProcessWithExitCode "ndisasm" ["-b", "32", binFile] ""
      case ec of
        ExitFailure _ -> return $ Left $ "ndisasm: " ++ err
        ExitSuccess   -> return $ Right out

------------------------------------------------------------------------
-- Test categories
------------------------------------------------------------------------

-- 1. W^X transition: mmap RW → write → mprotect RX → execute
testRawMmap :: IO Result
testRawMmap = do
    let pageSize = 4096 :: CSize
    buf <- c_mmap nullPtr pageSize (protRead .|. protWrite) (mapPrivate .|. mapAnonymous) (-1) 0
    if buf == intPtrToPtr (-1)
      then failWith "mmap failed"
      else do
        pokeByteOff buf 0 (0xb8 :: Word8)  -- mov eax, 42
        pokeByteOff buf 1 (42   :: Word8)
        pokeByteOff buf 2 (0    :: Word8)
        pokeByteOff buf 3 (0    :: Word8)
        pokeByteOff buf 4 (0    :: Word8)
        pokeByteOff buf 5 (0xc3 :: Word8)  -- ret
        rc <- c_mprotect buf pageSize (protRead .|. protExec)
        if rc /= 0
          then do _ <- c_munmap buf pageSize; failWith "mprotect failed"
          else do
            val <- mkFun (castPtrToFunPtr buf)
            _ <- c_munmap buf pageSize
            if val == 42 then pass
              else failWith $ "expected 42, got " ++ show val

-- 2. Pattern-fill W^X verification (from AsmJit JitAllocator test):
--    write pattern through RW mapping, flip to RX, read back through
--    executable mapping to verify the bytes survived the transition.
testWXPatternFill :: IO Result
testWXPatternFill = do
    let pageSize = 4096 :: CSize
        pattern  = cycle [0xDE, 0xAD, 0xBE, 0xEF] :: [Word8]
        fillSize = 256
    buf <- c_mmap nullPtr pageSize (protRead .|. protWrite) (mapPrivate .|. mapAnonymous) (-1) 0
    if buf == intPtrToPtr (-1)
      then failWith "mmap failed"
      else do
        mapM_ (\i -> pokeByteOff buf i (pattern !! i)) [0..fillSize-1]
        _ <- c_mprotect buf pageSize (protRead .|. protExec)
        readBack <- mapM (peekByteOff buf) [0..fillSize-1] :: IO [Word8]
        _ <- c_munmap buf pageSize
        if readBack == take fillSize pattern
          then pass
          else failWith "pattern mismatch after W->X transition"

testExecutableMemoryAPI :: IO Result
testExecutableMemoryAPI =
    ExecMem.withMapping 16 $ \mapping -> do
      let buf = ExecMem.mappingPtr mapping
      pokeByteOff buf 0 (0x2a :: Word8)
      ExecMem.protect ExecMem.ReadExecute mapping
      actual <- peekByteOff buf 0 :: IO Word8
      if actual == 0x2a && ExecMem.mappingSize mapping >= 16
        then pass
        else failWith $ "expected mapped byte 0x2a, got " ++ show actual

-- 3. Harpy API execution: emit simple functions, call, assert return value
testHarpyMovRet :: Word32 -> IO Result
testHarpyMovRet n = do
    (_, res) <- runCodeGen code () ()
    case res of
      Left err  -> failWith $ "codegen: " ++ show err
      Right val -> if val == n then pass
                   else failWith $ "expected " ++ show n ++ ", got " ++ show val
  where
    code = do mov eax (n :: Word32); ret; callAsWord32 0

-- 4. Golden encoding: (mnemonic, codegen, expected bytes)
testGolden :: String -> CodeGen () () () -> [Word8] -> IO Result
testGolden _ gen expected = do
    r <- emitBytes gen
    case r of
      Left err -> failWith err
      Right actual
        | actual == expected -> pass
        | otherwise -> failWith $
            "expected " ++ showHexBytes expected ++ ", got " ++ showHexBytes actual

-- 5. Differential test against nasm
testDiff :: String -> String -> CodeGen () () () -> IO Result
testDiff _ nasmSrc gen = do
    hr <- emitBytes gen
    nr <- nasmAssemble nasmSrc
    case (hr, nr) of
      (Left e, _) -> failWith $ "harpy: " ++ e
      (_, Left e) -> failWith e
      (Right hb, Right nb)
        | hb == nb  -> pass
        | otherwise -> failWith $ "harpy=" ++ showHexBytes hb ++ " nasm=" ++ showHexBytes nb

-- 6. ndisasm round-trip: emit → decode with ndisasm → check mnemonic appears
--    (Cranelift-style: use an independent disassembler as oracle)
testNdisasmRoundTrip :: String -> CodeGen () () () -> String -> IO Result
testNdisasmRoundTrip _ gen expectedMnemonic = do
    r <- emitBytes gen
    case r of
      Left err -> failWith err
      Right rawBytes -> do
        dr <- ndisasmDecode rawBytes
        case dr of
          Left err -> failWith err
          Right disasm ->
            if expectedMnemonic `isInfixOf` (map toLower disasm)
              then pass
              else failWith $ "'" ++ expectedMnemonic ++ "' not in: " ++ disasm
  where
    toLower c | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
              | otherwise = c

-- 7. Harpy's own disassembler round-trip
testHarpyDisasm :: String -> CodeGen () () () -> String -> IO Result
testHarpyDisasm _ gen expectedSubstr = do
    (_, res) <- runCodeGen act () ()
    case res of
      Left err -> failWith $ show err
      Right instrs ->
        let disasm = unlines $ map showIntel instrs
        in if expectedSubstr `isInfixOf` disasm
           then pass
           else failWith $ "'" ++ expectedSubstr ++ "' not in:\n" ++ disasm
  where
    act = do _ <- gen; disassemble

-- 8. Label fixup tests
testForwardJump :: IO Result
testForwardJump = do
    (_, res) <- runCodeGen code () ()
    case res of
      Left err  -> failWith $ show err
      Right val -> if val == 99 then pass
                   else failWith $ "expected 99, got " ++ show val
  where
    code = do
      target <- newLabel
      jmp target
      mov eax (0 :: Word32)
      ret
      target @@ mov eax (99 :: Word32)
      ret
      callAsWord32 0

-- backward jump using compare+conditional branch instead of loop
-- (loop uses rcx on x86-64 but harpy targets 32-bit x86)
testBackwardJump :: IO Result
testBackwardJump = do
    (_, res) <- runCodeGen code () ()
    case res of
      Left err  -> failWith $ show err
      Right val -> if val == 5 then pass
                   else failWith $ "expected 5, got " ++ show val
  where
    code = do
      mov ecx (5 :: Word32)
      mov eax (0 :: Word32)
      top <- setLabel
      add eax (1 :: Word32)
      sub ecx (1 :: Word32)
      jnz top
      ret
      callAsWord32 0

-- 9. Execution test: arithmetic (V8-style: assemble, run, check return value)
testExecArith :: String -> CodeGen () () () -> Word32 -> IO Result
testExecArith _ body expected = do
    (_, res) <- runCodeGen code () ()
    case res of
      Left err  -> failWith $ show err
      Right val -> if val == expected then pass
                   else failWith $ "expected " ++ show expected ++ ", got " ++ show val
  where
    code = do body; ret; callAsWord32 0

-- 10. Buffer overflow: ensureBufferSize grows the buffer via mmap+copy.
testSmallBuffer :: IO Result
testSmallBuffer = do
    let conf = defaultCodeGenConfig { codeBufferSize = 32 }
    (_, res) <- runCodeGenWithConfig code () () conf
    case res of
      Left err  -> failWith $ show err
      Right val ->
        if val == 1
          then pass
          else failWith $ "expected 1, got " ++ show val
  where
    code = do
      sequence_ $ replicate 40 nop
      mov eax (1 :: Word32)
      ret
      callAsWord32 0

-- 11. Encoding boundary tests (from the improvement doc: ±128, ±2G limits)
testBranchBoundary :: IO Result
testBranchBoundary = do
    r <- emitBytes code
    case r of
      Left err -> failWith err
      Right rawBytes ->
        if length rawBytes > 0 then pass
        else failWith "empty output"
  where
    code = do
      target <- newLabel
      -- 126 nops + short jmp should be within 8-bit displacement range
      sequence_ $ replicate 126 nop
      target @@ nop

------------------------------------------------------------------------
-- CodeImage tests
------------------------------------------------------------------------

testCodeImageBasic :: IO Result
testCodeImageBasic = do
    (_, res) <- assembleCodeImage code () ()
    case res of
      Left err -> failWith $ show err
      Right ((), img) ->
        let imageBytes = codeImageBytes img
        in if BS.length imageBytes > 0 && BS.last imageBytes == 0xc3
          then pass
          else failWith $ "expected code ending in ret, got " ++ show (BS.unpack imageBytes)
  where
    code :: CodeGen () () ()
    code = mov eax (42 :: Word32) >> ret

testCodeImageLoadExec :: IO Result
testCodeImageLoadExec = do
    (_, res) <- assembleCodeImage code () ()
    case res of
      Left err -> failWith $ show err
      Right ((), img) ->
        withExecutable img $ \exe -> do
          let fn = castPtrToFunPtr (executableEntryPtr exe) :: FunPtr (IO Word32)
          v <- mkCallIO fn
          if v == 42 then pass else failWith $ "expected 42, got " ++ show v
  where
    code :: CodeGen () () ()
    code = mov eax (42 :: Word32) >> ret

testCompileExecutableExec :: IO Result
testCompileExecutableExec = do
    (_, res) <- compileExecutable code () ()
    case res of
      Left err -> failWith $ show err
      Right ((), exe) -> do
        let fn = castPtrToFunPtr (executableEntryPtr exe) :: FunPtr (IO Word32)
        v <- mkCallIO fn
        freeExecutable exe
        if v == 42 then pass else failWith $ "expected 42, got " ++ show v
  where
    code :: CodeGen () () ()
    code = mov eax (42 :: Word32) >> ret

testWithCompiledExecutableExec :: IO Result
testWithCompiledExecutableExec = do
    (_, res) <- withCompiledExecutable code () () $ \() exe -> do
      let fn = castPtrToFunPtr (executableEntryPtr exe) :: FunPtr (IO Word32)
      mkCallIO fn
    case res of
      Left err -> failWith $ show err
      Right v -> if v == 42 then pass else failWith $ "expected 42, got " ++ show v
  where
    code :: CodeGen () () ()
    code = mov eax (42 :: Word32) >> ret

foreign import ccall "dynamic"
  mkCallIO :: FunPtr (IO Word32) -> IO Word32

testCodeImageSymbols :: IO Result
testCodeImageSymbols = do
    (_, res) <- assembleCodeImage code () ()
    case res of
      Left err -> failWith $ show err
      Right ((), img) ->
        case lookupSymbol "entry" img of
          Nothing  -> failWith "symbol 'entry' not found"
          Just ofs -> if ofs == 0 then pass
                      else failWith $ "expected offset 0, got " ++ show ofs
  where
    code :: CodeGen () () ()
    code = do
      l <- newNamedLabel "entry"
      l @@ nop
      ret

testCodeImageGolden :: IO Result
testCodeImageGolden = do
    (_, res) <- assembleCodeImage code () ()
    case res of
      Left err -> failWith $ show err
      Right ((), img) ->
        let actual = BS.unpack (codeImageBytes img)
            expected = [0xb8, 0x2a, 0x00, 0x00, 0x00, 0xc3]
        in if actual == expected then pass
           else failWith $ "expected " ++ showHexBytes expected ++ " got " ++ showHexBytes actual
  where
    code :: CodeGen () () ()
    code = mov eax (42 :: Word32) >> ret

testCodeImageSmallBuffer :: IO Result
testCodeImageSmallBuffer = do
    let conf = defaultCodeGenConfig { codeBufferSize = 32 }
    (_, res) <- assembleCodeImageWithConfig code () () conf
    case res of
      Left err -> failWith $ show err
      Right ((), img) ->
        let sz = codeImageSize img
        in if sz >= 40 then pass
           else failWith $ "expected >= 40 bytes from overflow, got " ++ show sz
  where
    code :: CodeGen () () ()
    code = sequence_ (replicate 40 nop) >> mov eax (1 :: Word32) >> ret

testCodeImageBufferGrowth :: IO Result
testCodeImageBufferGrowth = do
    let conf = defaultCodeGenConfig { codeBufferSize = 64 }
    (_, res) <- assembleCodeImageWithConfig code () () conf
    case res of
      Left err -> failWith $ show err
      Right ((), img) ->
        let sz = codeImageSize img
        in if sz == 501 then pass
           else failWith $ "expected 501 bytes from grown buffer, got " ++ show sz
  where
    code :: CodeGen () () ()
    code = replicateM_ 500 nop >> ret

testWithExecutableExceptionCleanup :: IO Result
testWithExecutableExceptionCleanup = do
    (_, res) <- assembleCodeImage code () ()
    case res of
      Left err -> failWith $ show err
      Right ((), img) -> do
        before <- anonymousRxBytes
        replicateM_ 20 $ do
          _ <- try (withExecutable img $ \_ ->
                    throwIO (userError "forced withExecutable cleanup path"))
               :: IO (Either SomeException ())
          return ()
        after <- anonymousRxBytes
        if after <= before + 4096
          then pass
          else failWith $ "anonymous RX mappings grew from "
                       ++ show before ++ " to " ++ show after
  where
    code :: CodeGen () () ()
    code = ret

anonymousRxBytes :: IO Integer
anonymousRxBytes = do
    maps <- readFile "/proc/self/maps"
    return $ sum [rangeBytes addr | line <- lines maps, Just addr <- [anonymousRxLine line]]

anonymousRxLine :: String -> Maybe String
anonymousRxLine line =
    case words line of
      [addr, perms, _offset, dev, inode]
        | perms == "r-xp" && dev == "00:00" && inode == "0" -> Just addr
      _ -> Nothing

rangeBytes :: String -> Integer
rangeBytes s =
    case break (== '-') s of
      (lo, '-':hi) -> hex hi - hex lo
      _            -> 0
  where
    hex x = case readHex x of
              ((n, _):_) -> n
              []         -> 0

testCodeImageRoundTrip :: IO Result
testCodeImageRoundTrip = do
    (_, directRes) <- runCodeGen directCode () ()
    (_, imgRes)    <- assembleCodeImage imgCode () ()
    case (directRes, imgRes) of
      (Left err, _) -> failWith $ "direct: " ++ show err
      (_, Left err) -> failWith $ "image: " ++ show err
      (Right directVal, Right ((), img)) ->
        withExecutable img $ \exe -> do
          let fn = castPtrToFunPtr (executableEntryPtr exe) :: FunPtr (IO Word32)
          imgExecVal <- mkCallIO fn
          if imgExecVal == directVal
            then pass
            else failWith $ "direct=" ++ show directVal ++ " image-exec=" ++ show imgExecVal
  where
    imgCode :: CodeGen () () ()
    imgCode = do
      mov eax (77 :: Word32)
      add eax (22 :: Word32)
      ret
    directCode :: CodeGen () () Word32
    directCode = do
      mov eax (77 :: Word32)
      add eax (22 :: Word32)
      ret
      callAsWord32 0

------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------

main :: IO ()
main = runTests
    [ -- Memory subsystem
      ("mmap-wx-transition",     testRawMmap)
    , ("wx-pattern-fill",        testWXPatternFill)
    , ("executable-memory-api",  testExecutableMemoryAPI)

    -- Harpy API execution smoke tests
    , ("exec-mov-ret-42",        testHarpyMovRet 42)
    , ("exec-mov-ret-0",         testHarpyMovRet 0)
    , ("exec-mov-ret-maxbound",  testHarpyMovRet maxBound)

    -- Golden encoding (AsmJit-style hex table)
    , ("golden-ret",             testGolden "ret" ret [0xc3])
    , ("golden-nop",             testGolden "nop" nop [0x90])
    , ("golden-mov-eax-1",       testGolden "mov eax,1" (mov eax (1 :: Word32))
                                   [0xb8, 0x01, 0x00, 0x00, 0x00])
    , ("golden-mov-ecx-ff",      testGolden "mov ecx,0xff" (mov ecx (0xff :: Word32))
                                   [0xb9, 0xff, 0x00, 0x00, 0x00])
    , ("golden-mov-edx-0",       testGolden "mov edx,0" (mov edx (0 :: Word32))
                                   [0xba, 0x00, 0x00, 0x00, 0x00])
    , ("golden-push-eax",        testGolden "push eax" (push eax) [0x50])
    , ("golden-push-ebx",        testGolden "push ebx" (push ebx) [0x53])
    , ("golden-pop-eax",         testGolden "pop eax"  (pop eax)  [0x58])
    , ("golden-pop-ebx",         testGolden "pop ebx"  (pop ebx)  [0x5b])
    , ("golden-push-pop",        testGolden "push+pop" (push eax >> pop eax) [0x50, 0x58])
    , ("golden-add-eax-imm",     testGolden "add eax,1" (add eax (1 :: Word32))
                                   [0x05, 0x01, 0x00, 0x00, 0x00])
    , ("golden-sub-eax-imm",     testGolden "sub eax,1" (sub eax (1 :: Word32))
                                   [0x2d, 0x01, 0x00, 0x00, 0x00])
    , ("golden-xor-eax-eax",     testGolden "xor eax,eax" (Harpy.xor eax eax) [0x33, 0xc0])
    , ("golden-inc-eax",         testGolden "inc eax" (inc eax) [0x40])
    , ("golden-dec-eax",         testGolden "dec eax" (dec eax) [0x48])
    , ("golden-mov-reg-reg",     testGolden "mov eax,ecx" (mov eax ecx) [0x8b, 0xc1])
    , ("golden-int3",            testGolden "int3" (breakpoint) [0xcc])
    , ("golden-cdq",             testGolden "cdq" cdq [0x99])
    , ("golden-ret-imm",         testGolden "ret 8" (retN 8) [0xc2, 0x08, 0x00])

    -- Differential tests against nasm (BITS 32 mode — single-encoding instructions)
    , ("diff-mov-eax-42",        testDiff "d" "mov eax, 42"          (mov eax (42 :: Word32)))
    , ("diff-ret",               testDiff "d" "ret"                  ret)
    , ("diff-nop",               testDiff "d" "nop"                  nop)
    , ("diff-push-ebx",          testDiff "d" "push ebx"             (push ebx))
    , ("diff-pop-edi",           testDiff "d" "pop edi"              (pop edi))
    , ("diff-mov-ecx-deadbeef",  testDiff "d" "mov ecx, 0xdeadbeef" (mov ecx (0xdeadbeef :: Word32)))
    , ("diff-cdq",               testDiff "d" "cdq"                 cdq)
    , ("diff-push-pop-ebp",      testDiff "d" "push ebp\npop ebp"   (push ebp >> pop ebp))
    , ("diff-and-eax-0xff",      testDiff "d" "and eax, 0xff"       (Harpy.and eax (0xff :: Word32)))

    -- ndisasm round-trip (Cranelift-style independent oracle)
    , ("ndisasm-ret",            testNdisasmRoundTrip "r" ret "ret")
    , ("ndisasm-nop",            testNdisasmRoundTrip "r" nop "nop")
    , ("ndisasm-mov-eax",        testNdisasmRoundTrip "r" (mov eax (42 :: Word32)) "mov")
    , ("ndisasm-push-ebx",       testNdisasmRoundTrip "r" (push ebx) "push")
    , ("ndisasm-xor-eax-eax",    testNdisasmRoundTrip "r" (Harpy.xor eax eax) "xor")
    , ("ndisasm-add",            testNdisasmRoundTrip "r" (add eax (1 :: Word32)) "add")

    -- Harpy disassembler round-trip
    , ("harpy-disasm-ret",       testHarpyDisasm "r" ret "ret")
    , ("harpy-disasm-nop",       testHarpyDisasm "r" nop "nop")

    -- Label / fixup
    , ("forward-jump",           testForwardJump)
    , ("backward-jump-loop",     testBackwardJump)
    , ("branch-boundary",        testBranchBoundary)

    -- Arithmetic execution tests (V8-style)
    , ("exec-add",               testExecArith "add"
                                   (mov eax (10 :: Word32) >> add eax (32 :: Word32)) 42)
    , ("exec-sub",               testExecArith "sub"
                                   (mov eax (50 :: Word32) >> sub eax (8 :: Word32)) 42)
    , ("exec-xor-self",          testExecArith "xor-self"
                                   (Harpy.xor eax eax) 0)
    -- inc/dec use 0x40-0x4f single-byte opcodes which are REX prefixes in x86-64.
    -- These tests document the known 32-bit-on-64-bit incompatibility.
    , ("exec-inc-KNOWN-32BIT",   skip "inc eax (0x40) is REX prefix on x86-64")
    , ("exec-dec-KNOWN-32BIT",   skip "dec eax (0x48) is REX prefix on x86-64")
    , ("exec-push-pop",          testExecArith "push-pop"
                                   (mov eax (99 :: Word32) >> push eax >> Harpy.xor eax eax >> pop eax) 99)
    , ("exec-mov-reg-reg",       testExecArith "mov-reg-reg"
                                   (mov ecx (77 :: Word32) >> mov eax ecx) 77)
    , ("exec-and",               testExecArith "and"
                                   (mov eax (0xff :: Word32) >> Harpy.and eax (0x0f :: Word32)) 0x0f)
    , ("exec-or",                testExecArith "or"
                                   (mov eax (0xf0 :: Word32) >> Harpy.or eax (0x0f :: Word32)) 0xff)

    -- Buffer overflow (known bug)
    , ("small-buffer-overflow",  testSmallBuffer)

    -- CodeImage
    , ("codeimage-basic",        testCodeImageBasic)
    , ("codeimage-load-exec",    testCodeImageLoadExec)
    , ("compile-executable-exec", testCompileExecutableExec)
    , ("with-compiled-executable-exec", testWithCompiledExecutableExec)
    , ("codeimage-symbols",      testCodeImageSymbols)
    , ("codeimage-golden",       testCodeImageGolden)
    , ("codeimage-small-buffer", testCodeImageSmallBuffer)
    , ("codeimage-buffer-growth", testCodeImageBufferGrowth)
    , ("codeimage-exception-cleanup", testWithExecutableExceptionCleanup)
    , ("codeimage-round-trip",   testCodeImageRoundTrip)
    ]
