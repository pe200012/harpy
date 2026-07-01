{-# LANGUAGE ForeignFunctionInterface #-}
module Harpy.X86_64.Call (
    -- * Invoke JIT-compiled code
      invoke
    , invokeI64
    , invokeI64_I64
    , invokeI64_I64_I64
    -- * Unsafe invoke (you cast)
    , unsafeInvoke
    ) where

import Data.Int
import Data.Word
import Foreign.Ptr

import Harpy.CodeGenMonad (CodeGen, assembleCodeImage)
import Harpy.CodeImage

-- | Assemble, load, execute as @IO Word64@, and free.
invoke :: CodeGen () () () -> IO Word64
invoke code = do
  (_, res) <- assembleCodeImage code () ()
  case res of
    Left err -> error ("invoke: " ++ show err)
    Right ((), img) -> withExecutable img $ \exe ->
      mkVoidW64 (castPtrToFunPtr (executableEntryPtr exe))

-- | Assemble and execute as @Int64 -> IO Int64@.
invokeI64 :: CodeGen () () () -> Int64 -> IO Int64
invokeI64 code arg = do
  (_, res) <- assembleCodeImage code () ()
  case res of
    Left err -> error ("invokeI64: " ++ show err)
    Right ((), img) -> withExecutable img $ \exe ->
      mkI64I64 (castPtrToFunPtr (executableEntryPtr exe)) arg

-- | Assemble and execute as @Int64 -> Int64 -> IO Int64@.
invokeI64_I64 :: CodeGen () () () -> Int64 -> Int64 -> IO Int64
invokeI64_I64 code a b = do
  (_, res) <- assembleCodeImage code () ()
  case res of
    Left err -> error ("invokeI64_I64: " ++ show err)
    Right ((), img) -> withExecutable img $ \exe ->
      mkI64I64I64 (castPtrToFunPtr (executableEntryPtr exe)) a b

-- | Assemble and execute as @Int64 -> Int64 -> Int64 -> IO Int64@.
invokeI64_I64_I64 :: CodeGen () () () -> Int64 -> Int64 -> Int64 -> IO Int64
invokeI64_I64_I64 code a b c = do
  (_, res) <- assembleCodeImage code () ()
  case res of
    Left err -> error ("invokeI64_I64_I64: " ++ show err)
    Right ((), img) -> withExecutable img $ \exe ->
      mkI64I64I64I64 (castPtrToFunPtr (executableEntryPtr exe)) a b c

-- | Assemble and invoke via an arbitrary FunPtr action. The caller is
-- responsible for providing a matching dynamic wrapper and running it
-- within the supplied action.
unsafeInvoke :: CodeGen () () () -> (FunPtr a -> IO b) -> IO b
unsafeInvoke code mk = do
  (_, res) <- assembleCodeImage code () ()
  case res of
    Left err -> error ("unsafeInvoke: " ++ show err)
    Right ((), img) -> withExecutable img $ \exe ->
      mk (castPtrToFunPtr (executableEntryPtr exe))

foreign import ccall "dynamic"
  mkVoidW64 :: FunPtr (IO Word64) -> IO Word64

foreign import ccall "dynamic"
  mkI64I64 :: FunPtr (Int64 -> IO Int64) -> Int64 -> IO Int64

foreign import ccall "dynamic"
  mkI64I64I64 :: FunPtr (Int64 -> Int64 -> IO Int64) -> Int64 -> Int64 -> IO Int64

foreign import ccall "dynamic"
  mkI64I64I64I64 :: FunPtr (Int64 -> Int64 -> Int64 -> IO Int64) -> Int64 -> Int64 -> Int64 -> IO Int64
