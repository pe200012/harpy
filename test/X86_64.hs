{-# LANGUAGE DataKinds, ScopedTypeVariables, TypeApplications,
             ForeignFunctionInterface, GADTs #-}
module Main (main) where

import Control.Monad (replicateM_)
import qualified Data.ByteString as BS
import Data.Word
import Data.Int
import Foreign hiding (xor)
import System.Exit
import System.IO
import System.Process
import Text.Printf

import Harpy.CodeGenMonad (CodeGen)
import Harpy.CodeImage
import Harpy.X86_64
import Harpy.X86_64.Macro
import Harpy.X86_64.Call (invoke, invokeI64, unsafeInvoke)

------------------------------------------------------------------------
-- Test harness
------------------------------------------------------------------------

data Result = Pass | Fail String | Skip String

runTests :: [(String, IO Result)] -> IO ()
runTests tests = do
    results <- mapM runOne tests
    let failed  = length [() | Fail _ <- results]
        skipped = length [() | Skip _ <- results]
        total   = length results
    putStrLn ""
    putStrLn $ show (total - failed - skipped) ++ " passed, "
            ++ show failed ++ " failed, "
            ++ show skipped ++ " skipped / " ++ show total ++ " total"
    if failed > 0 then exitFailure else exitSuccess
  where
    runOne (name, act) = do
        hFlush stdout
        r <- act
        case r of
          Pass   -> putStrLn $ "  OK   " ++ name
          Fail e -> putStrLn $ "  FAIL " ++ name ++ ": " ++ e
          Skip e -> putStrLn $ "  SKIP " ++ name ++ ": " ++ e
        hFlush stdout
        return r

pass :: IO Result
pass = pure Pass

failWith :: String -> IO Result
failWith = pure . Fail

showHex :: [Word8] -> String
showHex = concatMap (printf "%02x")

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

-- Emit code using the x86-64 assembler, return bytes
emitBytes :: CodeGen () () () -> IO (Either String [Word8])
emitBytes code = do
    (_, res) <- assembleCodeImage code () ()
    case res of
      Left err -> return (Left (show err))
      Right ((), img) -> return (Right (BS.unpack (codeImageBytes img)))

-- Assemble with nasm in 64-bit mode
nasmAssemble64 :: String -> IO (Either String [Word8])
nasmAssemble64 asmText = do
    let srcFile = "/tmp/harpy_x64_test.asm"
        outFile = "/tmp/harpy_x64_test.bin"
    writeFile srcFile $ "BITS 64\n" ++ asmText ++ "\n"
    (ec, _, err) <- readProcessWithExitCode "nasm" ["-f", "bin", "-o", outFile, srcFile] ""
    case ec of
      ExitFailure _ -> return $ Left $ "nasm: " ++ err
      ExitSuccess   -> Right . BS.unpack <$> BS.readFile outFile

-- Golden test: emit code, compare against expected bytes
testGolden :: String -> CodeGen () () () -> [Word8] -> IO Result
testGolden _desc code expected = do
    r <- emitBytes code
    case r of
      Left err -> failWith err
      Right actual
        | actual == expected -> pass
        | otherwise -> failWith $
            "expected " ++ showHex expected ++ " got " ++ showHex actual

-- Differential test: compare harpy output against nasm 64-bit
testDiff :: String -> CodeGen () () () -> IO Result
testDiff nasmCode harpyCode = do
    hr <- emitBytes harpyCode
    nr <- nasmAssemble64 nasmCode
    case (hr, nr) of
      (Left err, _) -> failWith $ "harpy: " ++ err
      (_, Left err) -> failWith $ "nasm: " ++ err
      (Right hb, Right nb)
        | hb == nb  -> pass
        | otherwise -> failWith $
            "harpy=" ++ showHex hb ++ " nasm=" ++ showHex nb

-- Execute code that returns a Word64 in RAX
foreign import ccall "dynamic"
  mkCallIO :: FunPtr (IO Word64) -> IO Word64

execCode :: CodeGen () () () -> IO Word64
execCode code = do
    (_, res) <- assembleCodeImage code () ()
    case res of
      Left err -> error (show err)
      Right ((), img) -> withExecutable img $ \exe ->
        mkCallIO (castPtrToFunPtr (executableEntryPtr exe))

testExec :: String -> CodeGen () () () -> Word64 -> IO Result
testExec _desc code expected = do
    actual <- execCode code
    if actual == expected
      then pass
      else failWith $ "expected " ++ show expected ++ " got " ++ show actual

------------------------------------------------------------------------
-- Tests
------------------------------------------------------------------------

main :: IO ()
main = runTests
    [ -- Golden encoding tests (verified against Intel manual / nasm)
      ("g-ret",         testGolden "ret" ret [0xc3])
    , ("g-nop",         testGolden "nop" nop [0x90])
    , ("g-syscall",     testGolden "syscall" syscall [0x0f, 0x05])
    , ("g-int3",        testGolden "int3" breakpoint [0xcc])
    , ("g-cdq",         testGolden "cdq" cdq [0x99])
    , ("g-cqo",         testGolden "cqo" cqo [0x48, 0x99])

    -- MOV reg, imm (32-bit operand)
    , ("g-mov-eax-42",  testGolden "mov eax,42"
        (mov (op eax) (imm 42))
        [0xb8, 0x2a, 0x00, 0x00, 0x00])
    -- MOV reg, imm (64-bit, fits imm32)
    , ("g-mov-rax-42",  testGolden "mov rax,42"
        (mov (op rax) (imm 42))
        [0x48, 0xc7, 0xc0, 0x2a, 0x00, 0x00, 0x00])

    -- PUSH/POP (64-bit default, no REX for base regs)
    , ("g-push-rax",    testGolden "push rax" (push rax) [0x50])
    , ("g-push-rbx",    testGolden "push rbx" (push rbx) [0x53])
    , ("g-pop-rax",     testGolden "pop rax"  (pop rax)  [0x58])
    , ("g-push-r8",     testGolden "push r8"  (push r8)  [0x41, 0x50])
    , ("g-pop-r15",     testGolden "pop r15"  (pop r15)  [0x41, 0x5f])

    -- ADD/SUB/XOR reg,reg (64-bit)
    , ("g-add-rax-rcx", testGolden "add rax,rcx"
        (add (op rax) (op rcx))
        [0x48, 0x01, 0xc8])  -- REX.W, 01 /r, ModRM(11,rcx,rax)
    , ("g-xor-rax-rax", testGolden "xor rax,rax"
        (xor (op rax) (op rax))
        [0x48, 0x31, 0xc0])
    , ("g-sub-r8-r9",   testGolden "sub r8,r9"
        (sub (op r8) (op r9))
        [0x4d, 0x29, 0xc8])  -- REX.WRB, 29 /r

    -- ADD reg,imm8 (sign-extended)
    , ("g-add-rax-1",   testGolden "add rax,1"
        (add (op rax) (imm 1))
        [0x48, 0x83, 0xc0, 0x01])
    , ("g-add-al-1",    testGolden "add al,1"
        (add (op al) (imm 1))
        [0x04, 0x01])
    , ("g-add-ax-1234", testGolden "add ax,0x1234"
        (add (op ax) (imm 0x1234))
        [0x66, 0x05, 0x34, 0x12])
    , ("g-mov-byte-mem-imm", testGolden "mov byte [rax],1"
        (mov (mem (base rax) :: Operand 'W8) (imm 1))
        [0xc6, 0x00, 0x01])
    , ("g-mov-word-mem-imm", testGolden "mov word [rax],0x1234"
        (mov (mem (base rax) :: Operand 'W16) (imm 0x1234))
        [0x66, 0xc7, 0x00, 0x34, 0x12])
    , ("g-shl-al-1",   testGolden "shl al,1"
        (shl (op al) (imm 1))
        [0xd0, 0xe0])

    -- INC/DEC (FF /0 /1 form)
    , ("g-inc-rax",     testGolden "inc rax"
        (inc (op rax))
        [0x48, 0xff, 0xc0])
    , ("g-inc-al",      testGolden "inc al"
        (inc (op al))
        [0xfe, 0xc0])
    , ("g-dec-r13",     testGolden "dec r13"
        (dec (op r13))
        [0x49, 0xff, 0xcd])

    -- 32-bit operations (no REX.W)
    , ("g-xor-eax-eax", testGolden "xor eax,eax"
        (xor (op eax) (op eax))
        [0x31, 0xc0])
    , ("g-add-eax-1",   testGolden "add eax,1"
        (add (op eax) (imm 1))
        [0x83, 0xc0, 0x01])
    , ("g-inc-eax",     testGolden "inc eax"
        (inc (op eax))
        [0xff, 0xc0])

    -- NEG/NOT
    , ("g-neg-rax",     testGolden "neg rax"
        (neg (op rax))
        [0x48, 0xf7, 0xd8])
    , ("g-not-rax",     testGolden "not rax"
        (Harpy.X86_64.not (op rax))
        [0x48, 0xf7, 0xd0])

    -- SHL/SHR
    , ("g-shl-rax-1",   testGolden "shl rax,1"
        (shl (op rax) (imm 1))
        [0x48, 0xd1, 0xe0])
    , ("g-shr-rax-4",   testGolden "shr rax,4"
        (shr (op rax) (imm 4))
        [0x48, 0xc1, 0xe8, 0x04])

    -- Differential tests against nasm (64-bit mode)
    , ("d-ret",         testDiff "ret" ret)
    , ("d-nop",         testDiff "nop" nop)
    , ("d-push-rbx",    testDiff "push rbx" (push rbx))
    , ("d-pop-rdi",     testDiff "pop rdi" (pop rdi))
    , ("d-push-r8",     testDiff "push r8" (push r8))
    , ("d-cdq",         testDiff "cdq" cdq)
    , ("d-cqo",         testDiff "cqo" cqo)
    , ("d-syscall",     testDiff "syscall" syscall)
    , ("d-inc-rax",     testDiff "inc rax" (inc (op rax)))
    , ("d-dec-r13",     testDiff "dec r13" (dec (op r13)))
    , ("d-xor-rax-rax", testDiff "xor rax, rax" (xor (op rax) (op rax)))
    , ("d-add-rax-1",   testDiff "add rax, 1" (add (op rax) (imm 1)))
    , ("d-neg-rax",     testDiff "neg rax" (neg (op rax)))
    , ("d-not-rax",     testDiff "not rax" (Harpy.X86_64.not (op rax)))
    -- nasm optimizes "mov rax, 42" to "mov eax, 42" (shorter, same result via zero-ext)
    -- so we skip this differential test — both encodings are correct
    , ("d-mov-eax-42",  testDiff "mov eax, 42" (mov (op eax) (imm 42)))
    , ("d-sub-r8-r9",   testDiff "sub r8, r9" (sub (op r8) (op r9)))
    , ("d-shl-rax-1",   testDiff "shl rax, 1" (shl (op rax) (imm 1)))
    , ("d-shr-rax-4",   testDiff "shr rax, 4" (shr (op rax) (imm 4)))

    -- Execution tests (we're on x86-64, these run natively)
    , ("x-mov-ret-42",  testExec "mov rax,42; ret"
        (mov (op rax) (imm 42) >> ret) 42)
    , ("x-add",         testExec "10+32=42"
        (mov (op rax) (imm 10) >> add (op rax) (imm 32) >> ret) 42)
    , ("x-sub",         testExec "50-8=42"
        (mov (op rax) (imm 50) >> sub (op rax) (imm 8) >> ret) 42)
    , ("x-xor-self",    testExec "xor rax,rax=0"
        (xor (op rax) (op rax) >> ret) 0)
    , ("x-inc",         testExec "inc 41=42"
        (mov (op rax) (imm 41) >> inc (op rax) >> ret) 42)
    , ("x-dec",         testExec "dec 43=42"
        (mov (op rax) (imm 43) >> dec (op rax) >> ret) 42)
    , ("x-neg",         testExec "neg -42=42"
        (mov (op rax) (imm (-42)) >> neg (op rax) >> ret) 42)
    , ("x-push-pop",    testExec "push/pop roundtrip"
        (mov (op rax) (imm 99) >> push rax >> xor (op rax) (op rax) >> pop rax >> ret) 99)
    , ("x-mov-reg-reg", testExec "mov rcx→rax"
        (mov (op rcx) (imm 77) >> mov (op rax) (op rcx) >> ret) 77)
    , ("x-shl",         testExec "1<<5=32"
        (mov (op rax) (imm 1) >> shl (op rax) (imm 5) >> ret) 32)
    , ("x-shr",         testExec "64>>1=32"
        (mov (op rax) (imm 64) >> shr (op rax) (imm 1) >> ret) 32)
    , ("x-and",         testExec "0xff & 0x0f = 0x0f"
        (mov (op rax) (imm 0xff) >> Harpy.X86_64.and (op rax) (imm 0x0f) >> ret) 0x0f)
    , ("x-or",          testExec "0xf0 | 0x0f = 0xff"
        (mov (op rax) (imm 0xf0) >> Harpy.X86_64.or (op rax) (imm 0x0f) >> ret) 0xff)
    , ("x-imul",        testExec "6*7=42"
        (mov (op rax) (imm 6) >> mov (op rcx) (imm 7) >> imul rax (op rcx) >> ret) 42)
    -- 64-bit: large value that doesn't fit 32-bit
    , ("x-mov-large",   testExec "mov rax, 0x100000000"
        (mov (op rax) (imm 0x100000000) >> ret) 0x100000000)
    -- Extended registers
    , ("x-r8-r9",       testExec "r8+r9=42"
        (mov (op r8) (imm 20) >> mov (op r9) (imm 22)
         >> mov (op rax) (op r8) >> add (op rax) (op r9) >> ret) 42)

    -- Branch relaxation tests
    , ("g-jcc-short-back", testGolden "backward jcc short (2 bytes)"
        (do lbl <- newLabel
            defineLabel lbl
            nop  -- 1 byte
            je lbl)
        [0x90, 0x74, 0xfd])  -- nop; je -3 (back over nop + 2-byte je)
    , ("x-loop-short",   testExec "loop with short backward jcc"
        (do mov (op rcx) (imm 10)
            xor (op rax) (op rax)
            lbl <- newLabel
            defineLabel lbl
            add (op rax) (imm 1)
            dec (op rcx)
            jne lbl
            ret) 10)
    , ("x-jmpLabel",     testExec "jmpLabel forward"
        (do lbl <- newLabel
            mov (op rax) (imm 99)
            jmpLabel lbl
            mov (op rax) (imm 0)
            defineLabel lbl
            ret) 99)
    , ("g-jmpLabel-short-back", testGolden "backward jmpLabel short"
        (do lbl <- newLabel
            defineLabel lbl
            jmpLabel lbl)
        [0xeb, 0xfe])  -- jmp -2 (jump to self)

    -- Macro tests (SysV64 ABI)
    , ("x-withFrame",   testExec "withFrame returns 42"
        (withFrame 0 $ mov (op rax) (imm 42)) 42)
    , ("x-callee-save", testExec "callee-saved regs preserved across frame"
        (do pushAll [rbx, r12]
            mov (op rbx) (imm 11)
            mov (op r12) (imm 31)
            mov (op rax) (op rbx)
            add (op rax) (op r12)
            popAll [rbx, r12]
            ret) 42)
    , ("x-frame-stack", testExec "prologue allocates stack space"
        (withFrame 32 $ do
            -- store 42 at [rbp-8], load it back
            mov (mem (disp rbp (-8))) (imm 42 :: Operand 'W64)
            mov (op rax) (mem (disp rbp (-8)))) 42)

    -- Typed invocation tests (Harpy.X86_64.Call)
    , ("x-invoke",      do
        r <- invoke (mov (op rax) (imm 42) >> ret)
        if r == 42 then pass else failWith $ "expected 42, got " ++ show r)
    , ("x-invokeI64",   do
        r <- invokeI64 (mov (op rax) (op rdi) >> add (op rax) (imm 1) >> ret) 41
        if r == 42 then pass else failWith $ "expected 42, got " ++ show r)
    , ("x-unsafeInvoke", do
        r <- unsafeInvoke (mov (op rax) (imm 42) >> ret) $ \fp ->
          mkCallIO (castFunPtr fp)
        if r == 42 then pass else failWith $ "expected 42, got " ++ show r)

    -- Verification tests
    , ("v-undef-label", do
        r <- emitBytes (do lbl <- newLabel; je lbl; ret)
        case r of
          Left _  -> pass
          Right _ -> failWith "expected failure for undefined label")

    -- Branch boundary tests
    , ("b-jcc-near-back", do
        -- 126 NOPs + backward je should use near (6-byte) form because
        -- the short displacement would be exactly -128 (out of range for
        -- the 2-byte branch + body, which is -(126+2) = -128).
        -- Actually -128 IS in range for rel8 (signed byte: -128..127).
        -- 127 NOPs would give -(127+2) = -129 which IS out of range.
        r <- emitBytes $ do
          lbl <- newLabel
          defineLabel lbl
          replicateM_ 127 nop  -- 127 bytes
          je lbl               -- displacement = -(127+2) = -129: near
        case r of
          Left err -> failWith err
          Right bs ->
            let len = length bs
                -- near form: 127 nops (127 bytes) + 6-byte jcc = 133
                -- short form: 127 nops + 2-byte jcc = 129
            in if len == 133 then pass
               else failWith $ "expected 133 bytes (near), got " ++ show len)

    , ("b-jcc-short-boundary", do
        -- 126 NOPs: displacement = -(126+2) = -128, which fits rel8
        r <- emitBytes $ do
          lbl <- newLabel
          defineLabel lbl
          replicateM_ 126 nop
          je lbl
        case r of
          Left err -> failWith err
          Right bs ->
            let len = length bs
            in if len == 128 then pass  -- 126 nops + 2-byte short je
               else failWith $ "expected 128 bytes (short), got " ++ show len)

    -- Additional instruction coverage
    , ("x-sar",         testExec "sar -8 >> 1 = -4"
        (mov (op rax) (imm (-8)) >> sar (op rax) (imm 1) >> ret)
        (fromIntegral (-4 :: Int64)))
    , ("x-test-jz",     testExec "test rax,rax; je taken"
        (do xor (op rax) (op rax)
            test (op rax) (op rax)
            lbl <- newLabel
            je lbl
            mov (op rax) (imm 1)
            defineLabel lbl
            mov (op rax) (imm 42)
            ret) 42)
    , ("x-cmp-jl",      testExec "cmp 5,10; jl taken"
        (do mov (op rax) (imm 5)
            cmp (op rax) (imm 10)
            lbl <- newLabel
            jl lbl
            mov (op rax) (imm 0)
            defineLabel lbl
            mov (op rax) (imm 42)
            ret) 42)
    , ("x-not",         testExec "not 0 = -1 (0xffffffffffffffff)"
        (xor (op rax) (op rax)
         >> Harpy.X86_64.not (op rax) >> ret) 0xffffffffffffffff)
    , ("x-idiv",        testExec "42 / 6 = 7"
        (do mov (op rax) (imm 42)
            xor (op rdx) (op rdx)  -- clear rdx for unsigned div
            mov (op rcx) (imm 6)
            idiv (op rcx)
            ret) 7)
    , ("x-lea",         testExec "lea rax, [rcx+rdx]"
        (do mov (op rcx) (imm 20)
            mov (op rdx) (imm 22)
            lea rax (Mem (Just rcx) (Just (rdx, S1)) 0)
            ret) 42)
    , ("x-retN",        testExec "retN 0 returns normally"
        (mov (op rax) (imm 42) >> retN 0) 42)
    , ("d-inc-eax",     testDiff "inc eax" (inc (op eax)))
    , ("d-neg-rcx",     testDiff "neg rcx" (neg (op rcx)))
    ]
