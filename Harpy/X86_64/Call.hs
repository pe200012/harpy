{-# LANGUAGE ForeignFunctionInterface #-}
module Harpy.X86_64.Call (
    -- * Invoke JIT-compiled code
      invoke
    , invokeI64
    , invokeI64_I64
    , invokeI64_I64_I64
    -- * Unsafe invoke
    , unsafeInvokeW64
    , unsafeInvokeI64
    , unsafeInvokeI64_I64
    , unsafeInvokeI64_I64_I64
    -- * Unsafe invoke (you cast)
    , unsafeInvoke
    ) where

import Data.Int
import Data.Word
import Foreign.Ptr

import Harpy.CodeGenMonad (CodeGen)
import Harpy.CodeImage

-- | Assemble, load, execute as @IO Word64@, and free.
invoke :: CodeGen () () () -> IO Word64
invoke code =
  runCompiled "invoke" code $ \exe ->
    mkVoidW64 (castPtrToFunPtr (executableEntryPtr exe))

-- | Assemble and execute as @Int64 -> IO Int64@.
invokeI64 :: CodeGen () () () -> Int64 -> IO Int64
invokeI64 code arg =
  runCompiled "invokeI64" code $ \exe ->
    mkI64I64 (castPtrToFunPtr (executableEntryPtr exe)) arg

-- | Assemble and execute as @Int64 -> Int64 -> IO Int64@.
invokeI64_I64 :: CodeGen () () () -> Int64 -> Int64 -> IO Int64
invokeI64_I64 code a b =
  runCompiled "invokeI64_I64" code $ \exe ->
    mkI64I64I64 (castPtrToFunPtr (executableEntryPtr exe)) a b

-- | Assemble and execute as @Int64 -> Int64 -> Int64 -> IO Int64@.
invokeI64_I64_I64 :: CodeGen () () () -> Int64 -> Int64 -> Int64 -> IO Int64
invokeI64_I64_I64 code a b c =
  runCompiled "invokeI64_I64_I64" code $ \exe ->
    mkI64I64I64I64 (castPtrToFunPtr (executableEntryPtr exe)) a b c

-- | Assemble and invoke via an arbitrary FunPtr action. The caller is
-- responsible for providing a matching dynamic wrapper and running it
-- within the supplied action.
unsafeInvoke :: CodeGen () () () -> (FunPtr a -> IO b) -> IO b
unsafeInvoke code mk =
  runCompiled "unsafeInvoke" code $ \exe ->
    mk (castPtrToFunPtr (executableEntryPtr exe))

-- | Assemble and execute as @IO Word64@ using an unsafe dynamic FFI call.
-- Use this only for short generated functions that never call back into
-- Haskell.
unsafeInvokeW64 :: CodeGen () () () -> IO Word64
unsafeInvokeW64 code =
  runCompiled "unsafeInvokeW64" code $ \exe ->
    mkUnsafeVoidW64 (castPtrToFunPtr (executableEntryPtr exe))

-- | Assemble and execute as @Int64 -> IO Int64@ using an unsafe dynamic
-- FFI call.
unsafeInvokeI64 :: CodeGen () () () -> Int64 -> IO Int64
unsafeInvokeI64 code arg =
  runCompiled "unsafeInvokeI64" code $ \exe ->
    mkUnsafeI64I64 (castPtrToFunPtr (executableEntryPtr exe)) arg

-- | Assemble and execute as @Int64 -> Int64 -> IO Int64@ using an unsafe
-- dynamic FFI call.
unsafeInvokeI64_I64 :: CodeGen () () () -> Int64 -> Int64 -> IO Int64
unsafeInvokeI64_I64 code a b =
  runCompiled "unsafeInvokeI64_I64" code $ \exe ->
    mkUnsafeI64I64I64 (castPtrToFunPtr (executableEntryPtr exe)) a b

-- | Assemble and execute as @Int64 -> Int64 -> Int64 -> IO Int64@ using
-- an unsafe dynamic FFI call.
unsafeInvokeI64_I64_I64 :: CodeGen () () () -> Int64 -> Int64 -> Int64 -> IO Int64
unsafeInvokeI64_I64_I64 code a b c =
  runCompiled "unsafeInvokeI64_I64_I64" code $ \exe ->
    mkUnsafeI64I64I64I64 (castPtrToFunPtr (executableEntryPtr exe)) a b c

runCompiled :: String -> CodeGen () () () -> (Executable -> IO a) -> IO a
runCompiled name code action = do
  (_, res) <- withCompiledExecutable code () () $ \() exe -> action exe
  case res of
    Left err -> error (name ++ ": " ++ show err)
    Right val -> return val

foreign import ccall "dynamic"
  mkVoidW64 :: FunPtr (IO Word64) -> IO Word64

foreign import ccall "dynamic"
  mkI64I64 :: FunPtr (Int64 -> IO Int64) -> Int64 -> IO Int64

foreign import ccall "dynamic"
  mkI64I64I64 :: FunPtr (Int64 -> Int64 -> IO Int64) -> Int64 -> Int64 -> IO Int64

foreign import ccall "dynamic"
  mkI64I64I64I64 :: FunPtr (Int64 -> Int64 -> Int64 -> IO Int64) -> Int64 -> Int64 -> Int64 -> IO Int64

foreign import ccall unsafe "dynamic"
  mkUnsafeVoidW64 :: FunPtr (IO Word64) -> IO Word64

foreign import ccall unsafe "dynamic"
  mkUnsafeI64I64 :: FunPtr (Int64 -> IO Int64) -> Int64 -> IO Int64

foreign import ccall unsafe "dynamic"
  mkUnsafeI64I64I64 :: FunPtr (Int64 -> Int64 -> IO Int64) -> Int64 -> Int64 -> IO Int64

foreign import ccall unsafe "dynamic"
  mkUnsafeI64I64I64I64 :: FunPtr (Int64 -> Int64 -> Int64 -> IO Int64) -> Int64 -> Int64 -> Int64 -> IO Int64
