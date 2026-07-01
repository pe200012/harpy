{-# LANGUAGE DataKinds, ScopedTypeVariables, TypeApplications,
             ForeignFunctionInterface #-}
-- | Examples demonstrating the Harpy x86-64 API.
--
-- Build:  cabal build && ghc -package harpy examples/X86_64_Examples.hs
-- Or:     cabal run x86-64-examples
module Main (main) where

import Prelude hiding (and, or, not)
import Data.Int
import Data.Word
import Foreign (peekByteOff)
import Foreign.Ptr
import Text.Printf

import Harpy.CodeGenMonad (CodeGen)
import Harpy.CodeImage
import Harpy.X86_64
import Harpy.X86_64.Macro
import Harpy.X86_64.Call (invoke, invokeI64)

------------------------------------------------------------------------
-- FFI wrappers for calling JIT code with different signatures
------------------------------------------------------------------------

foreign import ccall "dynamic"
  mkI64_I64 :: FunPtr (Int64 -> IO Int64) -> Int64 -> IO Int64

foreign import ccall "dynamic"
  mkI64_I64_I64 :: FunPtr (Int64 -> Int64 -> IO Int64)
                -> Int64 -> Int64 -> IO Int64

-- Helper: assemble, load, execute with given wrapper, print result
runWith :: String -> CodeGen () () () -> (FunPtr a -> b) -> (b -> IO c) -> IO c
runWith name code castFn callFn = do
  (_, res) <- assembleCodeImage code () ()
  case res of
    Left err -> error $ name ++ ": " ++ show err
    Right ((), img) -> do
      putStrLn $ name ++ ": " ++ show (codeImageSize img) ++ " bytes"
      withExecutable img $ \exe -> callFn (castFn (castPtrToFunPtr (executableEntryPtr exe)))

------------------------------------------------------------------------
-- Example 1: Hello, JIT — return a constant
------------------------------------------------------------------------

ex1_constant :: IO ()
ex1_constant = do
  putStrLn "\n=== Example 1: Return a constant ==="
  result <- invoke $ do
    mov (op rax) (imm 42)
    ret
  printf "  Result: %d (expected 42)\n" (fromIntegral result :: Int)

------------------------------------------------------------------------
-- Example 2: Arithmetic — compute (a + b) * 2
------------------------------------------------------------------------

ex2_arithmetic :: IO ()
ex2_arithmetic = do
  putStrLn "\n=== Example 2: Arithmetic (a + b) * 2 ==="
  let code = do
        -- SysV64: rdi = a, rsi = b
        mov (op rax) (op rdi)    -- rax = a
        add (op rax) (op rsi)    -- rax = a + b
        shl (op rax) (imm 1)    -- rax *= 2
        ret
  result <- runWith "arith" code mkI64_I64_I64 (\f -> f 10 11)
  printf "  (10 + 11) * 2 = %d (expected 42)\n" result

------------------------------------------------------------------------
-- Example 3: Branching — absolute value
------------------------------------------------------------------------

ex3_abs :: IO ()
ex3_abs = do
  putStrLn "\n=== Example 3: Absolute value ==="
  let code = do
        -- rdi = input
        mov (op rax) (op rdi)
        cmp (op rax) (imm 0)
        done <- newLabel
        jge done              -- if rax >= 0, skip neg
        neg (op rax)
        defineLabel done
        ret
  result <- runWith "abs" code mkI64_I64 (\f -> f (-42))
  printf "  abs(-42) = %d (expected 42)\n" result

------------------------------------------------------------------------
-- Example 4: Loops — sum 1..n
------------------------------------------------------------------------

ex4_sum :: IO ()
ex4_sum = do
  putStrLn "\n=== Example 4: Sum 1..n (loop) ==="
  let code = do
        -- rdi = n
        xor (op rax) (op rax)   -- sum = 0
        mov (op rcx) (op rdi)   -- counter = n
        loop <- newLabel
        defineLabel loop
        add (op rax) (op rcx)   -- sum += counter
        dec (op rcx)            -- counter--
        jne loop                -- short backward jump (auto-relaxed)
        ret
  result <- runWith "sum" code mkI64_I64 (\f -> f 100)
  printf "  sum(1..100) = %d (expected 5050)\n" result

------------------------------------------------------------------------
-- Example 5: Stack frame — using prologue/epilogue
------------------------------------------------------------------------

ex5_frame :: IO ()
ex5_frame = do
  putStrLn "\n=== Example 5: Stack frame with local variable ==="
  result <- invoke $ withFrame 16 $ do
    -- Store 21 at [rbp-8], double it, return
    mov (mem (disp rbp (-8))) (imm 21 :: Operand 'W64)
    mov (op rax) (mem (disp rbp (-8)))
    add (op rax) (op rax)
  printf "  21 * 2 = %d (expected 42)\n" (fromIntegral result :: Int)

------------------------------------------------------------------------
-- Example 6: Callee-saved registers
------------------------------------------------------------------------

ex6_callee_save :: IO ()
ex6_callee_save = do
  putStrLn "\n=== Example 6: Callee-saved registers ==="
  result <- invoke $ do
    pushAll [rbx, r12]
    mov (op rbx) (imm 20)
    mov (op r12) (imm 22)
    mov (op rax) (op rbx)
    add (op rax) (op r12)
    popAll [rbx, r12]
    ret
  printf "  20 + 22 = %d (expected 42)\n" (fromIntegral result :: Int)

------------------------------------------------------------------------
-- Example 7: invokeI64 — one-shot function with argument
------------------------------------------------------------------------

ex7_invoke :: IO ()
ex7_invoke = do
  putStrLn "\n=== Example 7: invokeI64 convenience ==="
  -- Factorial-like: n * (n-1) * ... * 1, but we'll just do n^2 for simplicity
  result <- invokeI64
    (do mov (op rax) (op rdi)   -- rax = n
        imul rax (op rdi)       -- rax = n * n
        ret)
    7
  printf "  7^2 = %d (expected 49)\n" result

------------------------------------------------------------------------
-- Example 8: Code inspection before execution
------------------------------------------------------------------------

ex8_inspect :: IO ()
ex8_inspect = do
  putStrLn "\n=== Example 8: Inspect CodeImage before executing ==="
  (_, res) <- assembleCodeImage
    (do mov (op rax) (imm 0xFF) >> ret)
    () ()
  case res of
    Left err -> putStrLn $ "  Error: " ++ show err
    Right ((), img) -> do
      let size = codeImageSize img
      printf "  Code size: %d bytes\n" size
      putStr "  Machine code: "
      withExecutable img $ \exe -> do
        let p = executableEntryPtr exe
        mapM_ (\i -> peekByteOff p i >>= printf "%02x " . (fromIntegral :: Word8 -> Int)) [0..size-1]
      putStrLn ""

------------------------------------------------------------------------
-- Example 9: Memory addressing modes
------------------------------------------------------------------------

ex9_memory :: IO ()
ex9_memory = do
  putStrLn "\n=== Example 9: Memory addressing (LEA) ==="
  result <- invoke $ do
    -- Compute 5 * 8 + 2 = 42 using LEA with scale
    mov (op rcx) (imm 5)
    lea rax (base rcx)                     -- lea rax, [rcx] (just base)
    -- rax = 5, now multiply by 8 using shift
    shl (op rax) (imm 3)                   -- rax = 40
    add (op rax) (imm 2)                   -- rax = 42
    ret
  printf "  5 * 8 + 2 = %d (expected 42)\n" (fromIntegral result :: Int)

------------------------------------------------------------------------
-- Example 10: Fibonacci
------------------------------------------------------------------------

ex10_fib :: IO ()
ex10_fib = do
  putStrLn "\n=== Example 10: Fibonacci ==="
  let code = do
        -- rdi = n; returns fib(n)
        -- fib(0)=0, fib(1)=1
        cmp (op rdi) (imm 1)
        done0 <- newLabel
        jle done0
        -- a=0 (r8), b=1 (r9), iterate n-1 times
        xor (op r8) (op r8)       -- a = 0
        mov (op r9) (imm 1)       -- b = 1
        mov (op rcx) (op rdi)     -- counter = n
        dec (op rcx)              -- n-1 iterations
        loop <- newLabel
        defineLabel loop
        mov (op rax) (op r9)      -- tmp = b
        add (op r9) (op r8)       -- b = a + b
        mov (op r8) (op rax)      -- a = tmp
        dec (op rcx)
        jne loop
        mov (op rax) (op r9)
        ret
        defineLabel done0
        mov (op rax) (op rdi)     -- fib(0)=0, fib(1)=1
        ret
  result <- runWith "fib" code mkI64_I64 (\f -> f 10)
  printf "  fib(10) = %d (expected 55)\n" result

------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "Harpy x86-64 Examples"
  putStrLn "====================="
  ex1_constant
  ex2_arithmetic
  ex3_abs
  ex4_sum
  ex5_frame
  ex6_callee_save
  ex7_invoke
  ex8_inspect
  ex9_memory
  ex10_fib
  putStrLn "\nAll examples complete."
