{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Main (main) where

import Prelude hiding (and, or, not)

import Control.Exception (bracket, evaluate)
import Control.DeepSeq (NFData(..))
import Criterion.Main
import Data.Int
import Data.IORef
import Data.Word
import qualified Data.Vector.Storable as VS
import Foreign.Marshal.Alloc (free)
import Foreign.Marshal.Array (mallocArray, pokeArray)
import Foreign.Ptr (FunPtr, Ptr, castPtrToFunPtr, plusPtr)
import Foreign.Storable (peek, peekElemOff)

import Harpy.CodeGenMonad (CodeGen)
import Harpy.CodeImage
import Harpy.X86_64

newtype BenchExecutable = BenchExecutable { benchExecutable :: Executable }

instance NFData BenchExecutable where
  rnf wrapped = executableEntryPtr (benchExecutable wrapped) `seq` ()

------------------------------------------------------------------------
-- Dynamic FFI wrappers
------------------------------------------------------------------------

foreign import ccall "dynamic"
  mkSafeI64_I64 :: FunPtr (Int64 -> IO Int64) -> Int64 -> IO Int64

foreign import ccall unsafe "dynamic"
  mkUnsafeI64_I64 :: FunPtr (Int64 -> IO Int64) -> Int64 -> IO Int64

foreign import ccall unsafe "dynamic"
  mkUnsafePtrI64_I64 :: FunPtr (Ptr Int64 -> Int64 -> IO Int64)
                     -> Ptr Int64 -> Int64 -> IO Int64

foreign import ccall unsafe "dynamic"
  mkUnsafePtrPtrI32_I64_W64 :: FunPtr (Ptr Int32 -> Ptr Int32 -> Int64 -> IO Word64)
                            -> Ptr Int32 -> Ptr Int32 -> Int64 -> IO Word64

------------------------------------------------------------------------
-- Benchmark environments
------------------------------------------------------------------------

loadJit :: CodeGen () () () -> IO BenchExecutable
loadJit code = do
  (_, res) <- compileExecutable code () ()
  case res of
    Left err -> error $ "JIT compile failed: " ++ show err
    Right ((), exe) -> return (BenchExecutable exe)

freeJit :: BenchExecutable -> IO ()
freeJit = freeExecutable . benchExecutable

withLoaded :: CodeGen () () () -> (Executable -> IO a) -> IO a
withLoaded code action = bracket (loadJit code) freeJit (action . benchExecutable)

data DotEnv = DotEnv
  { dotPtr      :: !(Ptr Int64)
  , dotVector   :: !(VS.Vector Int64)
  , dotCount    :: !Int
  , dotExpected :: !Int64
  , dotJit      :: !BenchExecutable
  , dotJit2     :: !BenchExecutable
  }

instance NFData DotEnv where
  rnf benchEnv =
    dotPtr benchEnv `seq`
    dotVector benchEnv `seq`
    dotCount benchEnv `seq`
    dotExpected benchEnv `seq`
    rnf (dotJit benchEnv) `seq`
    rnf (dotJit2 benchEnv) `seq`
    ()

mkDotEnv :: Int -> IO DotEnv
mkDotEnv n = do
  let xs = [if even i then fromIntegral (i `div` 2 + 1) else 1 | i <- [0 .. n * 2 - 1]]
      expected = hsSum (fromIntegral n)
      vec = VS.fromList xs
  arr <- mallocArray (n * 2)
  pokeArray arr xs
  exe <- loadJit jitDotCode
  exe2 <- loadJit jitDotUnrolledCode
  v1 <- mkUnsafePtrI64_I64 (castPtrToFunPtr (executableEntryPtr (benchExecutable exe))) arr (fromIntegral n)
  v2 <- mkUnsafePtrI64_I64 (castPtrToFunPtr (executableEntryPtr (benchExecutable exe2))) arr (fromIntegral n)
  if v1 == expected && v2 == expected && hsDotVector vec == expected
    then return DotEnv
      { dotPtr = arr
      , dotVector = vec
      , dotCount = n
      , dotExpected = expected
      , dotJit = exe
      , dotJit2 = exe2
      }
    else do
      freeJit exe
      freeJit exe2
      free arr
      error "dot-product benchmark validation failed"

freeDotEnv :: DotEnv -> IO ()
freeDotEnv benchEnv = do
  freeJit (dotJit benchEnv)
  freeJit (dotJit2 benchEnv)
  free (dotPtr benchEnv)

data DotI32Env = DotI32Env
  { dotI32A        :: !(Ptr Int32)
  , dotI32B        :: !(Ptr Int32)
  , dotI32Count    :: !Int64
  , dotI32Expected :: !Word64
  , dotI32Jit      :: !BenchExecutable
  }

instance NFData DotI32Env where
  rnf benchEnv =
    dotI32A benchEnv `seq`
    dotI32B benchEnv `seq`
    dotI32Count benchEnv `seq`
    dotI32Expected benchEnv `seq`
    rnf (dotI32Jit benchEnv) `seq`
    ()

mkDotI32Env :: Int -> IO DotI32Env
mkDotI32Env n = do
  let as = map fromIntegral [1 .. n] :: [Int32]
      bs = replicate n 1 :: [Int32]
      expected = fromIntegral (hsSum (fromIntegral n))
  pa <- mallocArray n
  pb <- mallocArray n
  pokeArray pa as
  pokeArray pb bs
  exe <- loadJit jitDotI32SSE41Code
  got <- mkUnsafePtrPtrI32_I64_W64 (castPtrToFunPtr (executableEntryPtr (benchExecutable exe))) pa pb (fromIntegral n)
  if got == expected
    then return DotI32Env
      { dotI32A = pa
      , dotI32B = pb
      , dotI32Count = fromIntegral n
      , dotI32Expected = expected
      , dotI32Jit = exe
      }
    else do
      freeJit exe
      free pa
      free pb
      error "SIMD dot-product benchmark validation failed"

freeDotI32Env :: DotI32Env -> IO ()
freeDotI32Env benchEnv = do
  freeJit (dotI32Jit benchEnv)
  free (dotI32A benchEnv)
  free (dotI32B benchEnv)

------------------------------------------------------------------------
-- Haskell baselines
------------------------------------------------------------------------

hsFib :: Int64 -> Int64
hsFib n
  | n <= 1 = n
  | otherwise = go 0 1 (n - 1)
  where
    go !_ !b 0 = b
    go !a !b k = go b (a + b) (k - 1)

hsSum :: Int64 -> Int64
hsSum n = go 0 n
  where
    go !acc 0 = acc
    go !acc k = go (acc + k) (k - 1)

hsDotPeekIndex :: Ptr Int64 -> Int -> IO Int64
hsDotPeekIndex _ 0 = return 0
hsDotPeekIndex p n = go 0 0
  where
    go !acc i
      | i >= n = return acc
      | otherwise = do
          a <- peekElemOff p (i * 2)
          b <- peekElemOff p (i * 2 + 1)
          go (acc + a * b) (i + 1)

hsDotPtrWalk :: Ptr Int64 -> Int -> IO Int64
hsDotPtrWalk p0 n0 = go 0 p0 n0
  where
    go !acc !_ 0 = return acc
    go !acc !p !n = do
      a <- peek p
      b <- peekElemOff p 1
      go (acc + a * b) (p `plusPtr` 16) (n - 1)

hsDotVector :: VS.Vector Int64 -> Int64
hsDotVector v = go 0 0
  where
    pairs = VS.length v `quot` 2
    go !acc !i
      | i >= pairs = acc
      | otherwise =
          let a = VS.unsafeIndex v (i * 2)
              b = VS.unsafeIndex v (i * 2 + 1)
          in go (acc + a * b) (i + 1)

hsCollatz :: Int64 -> Int64
hsCollatz n = go n 0
  where
    go 1 !steps = steps
    go k !steps
      | even k = go (k `quot` 2) (steps + 1)
      | otherwise = go (3 * k + 1) (steps + 1)

hsIncLoop :: Int64 -> Int64
hsIncLoop n = go 0 n
  where
    go !acc 0 = acc
    go !acc k = go (acc + 1) (k - 1)

readApply :: IORef Int64 -> (Int64 -> Int64) -> IO Int64
readApply ref f = do
  n <- readIORef ref
  evaluate $ f n

------------------------------------------------------------------------
-- JIT programs
------------------------------------------------------------------------

jitFibCode :: CodeGen () () ()
jitFibCode = do
  cmp (op rdi) (imm 1)
  done0 <- newLabel
  jle done0
  xor (op r8) (op r8)
  mov (op r9) (imm 1)
  mov (op rcx) (op rdi)
  dec (op rcx)
  loop <- newLabel
  defineLabel loop
  mov (op rax) (op r9)
  add (op r9) (op r8)
  mov (op r8) (op rax)
  dec (op rcx)
  jne loop
  mov (op rax) (op r9)
  ret
  defineLabel done0
  mov (op rax) (op rdi)
  ret

jitSumCode :: CodeGen () () ()
jitSumCode = do
  xor (op rax) (op rax)
  mov (op rcx) (op rdi)
  loop <- newLabel
  defineLabel loop
  add (op rax) (op rcx)
  dec (op rcx)
  jne loop
  ret

jitDotCode :: CodeGen () () ()
jitDotCode = do
  xor (op rax) (op rax)
  test (op rsi) (op rsi)
  done <- newLabel
  je done
  mov (op rcx) (op rsi)
  mov (op r10) (op rdi)
  loop <- newLabel
  defineLabel loop
  mov (op r8) (mem (base r10))
  mov (op r9) (mem (disp r10 8))
  imul r8 (op r9)
  add (op rax) (op r8)
  add (op r10) (imm 16)
  dec (op rcx)
  jne loop
  defineLabel done
  ret

jitDotUnrolledCode :: CodeGen () () ()
jitDotUnrolledCode = do
  xor (op rax) (op rax)
  xor (op r11) (op r11)
  mov (op r10) (op rdi)
  mov (op rcx) (op rsi)
  shr (op rcx) (imm 1)
  tailPairs <- newLabel
  je tailPairs
  loop <- newLabel
  defineLabel loop
  mov (op r8) (mem (base r10))
  mov (op r9) (mem (disp r10 8))
  imul r8 (op r9)
  add (op rax) (op r8)
  mov (op r8) (mem (disp r10 16))
  mov (op r9) (mem (disp r10 24))
  imul r8 (op r9)
  add (op r11) (op r8)
  add (op r10) (imm 32)
  dec (op rcx)
  jne loop
  add (op rax) (op r11)
  defineLabel tailPairs
  test (op rsi) (imm 1)
  done <- newLabel
  je done
  mov (op r8) (mem (base r10))
  mov (op r9) (mem (disp r10 8))
  imul r8 (op r9)
  add (op rax) (op r8)
  defineLabel done
  ret

jitDotI32SSE41Code :: CodeGen () () ()
jitDotI32SSE41Code = do
  pxorX xmm0 xmm0
  mov (op rcx) (op rdx)
  shr (op rcx) (imm 2)
  tailValues <- newLabel
  je tailValues
  loop <- newLabel
  defineLabel loop
  movdquLoad xmm1 (base rdi)
  movdquLoad xmm2 (base rsi)
  pmulld xmm1 xmm2
  paddd xmm0 xmm1
  add (op rdi) (imm 16)
  add (op rsi) (imm 16)
  dec (op rcx)
  jne loop
  defineLabel tailValues
  movdquXmm xmm1 xmm0
  psrldq xmm1 8
  paddd xmm0 xmm1
  movdquXmm xmm1 xmm0
  psrldq xmm1 4
  paddd xmm0 xmm1
  movdFromXmm eax xmm0
  mov (op rcx) (op rdx)
  and (op rcx) (imm 3)
  done <- newLabel
  je done
  scalar <- newLabel
  defineLabel scalar
  mov (op r8d) (mem (base rdi))
  mov (op r9d) (mem (base rsi))
  imul r8d (op r9d)
  add (op eax) (op r8d)
  add (op rdi) (imm 4)
  add (op rsi) (imm 4)
  dec (op rcx)
  jne scalar
  defineLabel done
  ret

jitCollatzCode :: CodeGen () () ()
jitCollatzCode = do
  mov (op rcx) (op rdi)
  xor (op rax) (op rax)
  done <- newLabel
  loop <- newLabel
  defineLabel loop
  cmp (op rcx) (imm 1)
  je done
  inc (op rax)
  test (op cl) (imm 1 :: Operand 'W8)
  oddPath <- newLabel
  jne oddPath
  shr (op rcx) (imm 1)
  jmpLabel loop
  defineLabel oddPath
  mov (op rdx) (op rcx)
  shl (op rcx) (imm 1)
  add (op rcx) (op rdx)
  inc (op rcx)
  jmpLabel loop
  defineLabel done
  ret

jitIncLoopCode :: CodeGen () () ()
jitIncLoopCode = do
  xor (op rax) (op rax)
  mov (op rcx) (op rdi)
  test (op rcx) (op rcx)
  done <- newLabel
  je done
  loop <- newLabel
  defineLabel loop
  inc (op rax)
  dec (op rcx)
  jne loop
  defineLabel done
  ret

------------------------------------------------------------------------
-- Benchmarks
------------------------------------------------------------------------

benchUnaryI64 :: String -> Int64 -> (Int64 -> Int64) -> CodeGen () () () -> Benchmark
benchUnaryI64 name input hs jitCode =
  bgroup name
    [ bench "haskell/static" $ nf hs input
    , env (newIORef input) $ \ref ->
        bench "haskell/dynamic-IORef" $ nfIO (readApply ref hs)
    , envWithCleanup (loadJit jitCode) freeJit $ \wrapped ->
        let fp = castPtrToFunPtr (executableEntryPtr (benchExecutable wrapped))
        in bench "jit/unsafe-dynamic" $ nfIO (mkUnsafeI64_I64 fp input)
    ]

benchIncLoop :: Benchmark
benchIncLoop =
  bgroup "inc-loop-100"
    [ bench "haskell/static" $ nf hsIncLoop n
    , env (newIORef n) $ \ref ->
        bench "haskell/dynamic-IORef" $ nfIO (readApply ref hsIncLoop)
    , envWithCleanup (loadJit jitIncLoopCode) freeJit $ \wrapped ->
        let fp = castPtrToFunPtr (executableEntryPtr (benchExecutable wrapped))
        in bgroup "jit"
          [ bench "safe-dynamic" $ nfIO (mkSafeI64_I64 fp n)
          , bench "unsafe-dynamic" $ nfIO (mkUnsafeI64_I64 fp n)
          ]
    ]
  where
    n = 100

benchDotI64 :: Benchmark
benchDotI64 =
  envWithCleanup (mkDotEnv 1000) freeDotEnv $ \benchEnv ->
    let fp = castPtrToFunPtr (executableEntryPtr (benchExecutable (dotJit benchEnv)))
        fp2 = castPtrToFunPtr (executableEntryPtr (benchExecutable (dotJit2 benchEnv)))
        ptr = dotPtr benchEnv
        count = dotCount benchEnv
        count64 = fromIntegral count
        vec = dotVector benchEnv
    in bgroup "dot-int64-1000"
      [ bench "haskell/peekElemOff-index" $ nfIO (hsDotPeekIndex ptr count)
      , bench "haskell/pointer-walk" $ nfIO (hsDotPtrWalk ptr count)
      , bench "haskell/vector-storable" $ nf hsDotVector vec
      , bench "jit/pointer-walk" $ nfIO (mkUnsafePtrI64_I64 fp ptr count64)
      , bench "jit/unrolled-2x" $ nfIO (mkUnsafePtrI64_I64 fp2 ptr count64)
      ]

benchDotI32SIMD :: Benchmark
benchDotI32SIMD =
  envWithCleanup (mkDotI32Env 1024) freeDotI32Env $ \benchEnv ->
    let fp = castPtrToFunPtr (executableEntryPtr (benchExecutable (dotI32Jit benchEnv)))
    in bgroup "dot-int32-sse41-1024"
      [ bench "jit/pmulld-paddd" $
          nfIO (mkUnsafePtrPtrI32_I64_W64 fp (dotI32A benchEnv) (dotI32B benchEnv) (dotI32Count benchEnv))
      ]

benchCompile :: Benchmark
benchCompile =
  bgroup "compile-fib"
    [ bench "assemble+load/free" $ nfIO $ do
        (_, res) <- assembleCodeImage jitFibCode () ()
        case res of
          Left err -> error (show err)
          Right ((), img) -> bracket (loadCodeImage img) freeExecutable $ \_ ->
            return (0 :: Int64)
    , bench "compileExecutable/free" $ nfIO $
        withLoaded jitFibCode $ \_ -> return (0 :: Int64)
    , bench "compileExecutable+run" $ nfIO $
        withLoaded jitFibCode $ \exe ->
          mkUnsafeI64_I64 (castPtrToFunPtr (executableEntryPtr exe)) 30
    ]

main :: IO ()
main = defaultMain
  [ benchUnaryI64 "fib-30" 30 hsFib jitFibCode
  , benchUnaryI64 "sum-1-to-10000" 10000 hsSum jitSumCode
  , benchDotI64
  , benchDotI32SIMD
  , benchUnaryI64 "collatz-837799" 837799 hsCollatz jitCollatzCode
  , benchIncLoop
  , benchCompile
  ]
