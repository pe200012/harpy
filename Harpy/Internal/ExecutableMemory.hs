{-# LANGUAGE ForeignFunctionInterface #-}

--------------------------------------------------------------------------
-- |
-- Module:      Harpy.Internal.ExecutableMemory
-- License:     BSD3
--
-- Small Unix executable-memory abstraction used by Harpy's JIT loaders.
-- It centralizes anonymous mmap allocation, page alignment, protection
-- transitions, and release so code generation and image loading share the
-- same ownership rules.
--------------------------------------------------------------------------

module Harpy.Internal.ExecutableMemory
  ( Mapping(..)
  , Protection(..)
  , pageAlign
  , allocate
  , protect
  , free
  , withMapping
  ) where

import Control.Exception (bracket)
import Data.Bits ((.&.), (.|.), complement)
import Data.Word (Word8)
import Foreign.C.Types (CInt(..), CLong(..), CSize(..))
import Foreign.Ptr (Ptr, intPtrToPtr, nullPtr)

-- | A page-aligned anonymous memory mapping.
data Mapping = Mapping
  { mappingPtr  :: !(Ptr Word8)
  , mappingSize :: !Int
  }

-- | Supported protection states for generated code.
data Protection
  = ReadWrite
  | ReadExecute
  deriving (Eq, Show)

foreign import ccall "sys/mman.h mmap"
  c_mmap :: Ptr () -> CSize -> CInt -> CInt -> CInt -> CLong -> IO (Ptr Word8)

foreign import ccall "sys/mman.h mprotect"
  c_mprotect :: Ptr Word8 -> CSize -> CInt -> IO CInt

foreign import ccall "sys/mman.h munmap"
  c_munmap :: Ptr Word8 -> CSize -> IO CInt

protRead, protWrite, protExec :: CInt
protRead  = 0x1
protWrite = 0x2
protExec  = 0x4

mapPrivate, mapAnonymous :: CInt
mapPrivate   = 0x2
mapAnonymous = 0x20

mapFailed :: Ptr Word8
mapFailed = intPtrToPtr (-1)

-- | Round a byte count up to the system page size used by Harpy.
pageAlign :: Int -> Int
pageAlign n = (n + 4095) .&. complement 4095

-- | Allocate an anonymous read-write mapping of at least the requested size.
allocate :: Int -> IO Mapping
allocate requestedSize = do
    let alignedSize = pageAlign (max requestedSize 1)
    ptr <- c_mmap nullPtr (fromIntegral alignedSize)
                  (protRead .|. protWrite)
                  (mapPrivate .|. mapAnonymous)
                  (-1) 0
    if ptr == mapFailed
      then ioError (userError "Harpy: mmap failed")
      else return Mapping { mappingPtr = ptr, mappingSize = alignedSize }

-- | Change a mapping's memory protection.
protect :: Protection -> Mapping -> IO ()
protect protection mapping = do
    let prot = case protection of
                 ReadWrite   -> protRead .|. protWrite
                 ReadExecute -> protRead .|. protExec
    rc <- c_mprotect (mappingPtr mapping)
                     (fromIntegral (pageAlign (mappingSize mapping)))
                     prot
    if rc /= 0
      then ioError (userError ("Harpy: mprotect " ++ show protection ++ " failed"))
      else return ()

-- | Release a mapping.
free :: Mapping -> IO ()
free mapping = do
    _ <- c_munmap (mappingPtr mapping)
                   (fromIntegral (pageAlign (mappingSize mapping)))
    return ()

-- | Allocate a mapping, run an action, and always release the mapping.
withMapping :: Int -> (Mapping -> IO a) -> IO a
withMapping size = bracket (allocate size) free
