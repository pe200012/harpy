--------------------------------------------------------------------------
-- |
-- Module:      Harpy.CodeImage
-- Copyright:   (c) 2006-2015 Martin Grabmueller and Dirk Kleeblatt
-- License:     BSD3
--
-- Maintainer:  martin@grabmueller.de
-- Stability:   provisional
-- Portability: non-portable
--
-- Architecture-independent representation of assembled code.
-- A 'CodeImage' captures the output of code generation as pure data
-- that can be inspected, serialized, or loaded into executable memory.
--------------------------------------------------------------------------

module Harpy.CodeImage(
    -- * Types (re-exported from CodeGenMonad)
      CodeImage(..)
    , Section(..)
    , SectionKind(..)
    , CISymbol(..)
    , Executable
    -- * Producing a CodeImage (re-exported from CodeGenMonad)
    , assembleCodeImage
    , assembleCodeImageWithConfig
    -- * Loading and executing
    , loadCodeImage
    , compileExecutable
    , withCompiledExecutable
    , withExecutable
    , freeExecutable
    , executableEntryPtr
    -- * Inspecting
    , codeImageBytes
    , codeImageSize
    , lookupSymbol
    -- * Debugging
    , writePerfMapEntry
    ) where

import Harpy.CodeGenMonad
    ( CodeImage(..), Section(..), SectionKind(..), CISymbol(..)
    , assembleCodeImage, assembleCodeImageWithConfig
    , CodeGen, ErrMsg, compileExecutableBuffer, withCompiledExecutableBuffer
    )

import Control.Exception (bracket)
import qualified Data.ByteString as BS
import Data.Bits
import Data.Word
import Foreign
import Foreign.C.Types
import Numeric (showHex)
import System.IO (hPutStr, hFlush, IOMode(..), withFile)
import System.Posix.Types (CPid(..))
import System.Posix.Process (getProcessID)

-- | An executable code region backed by mmap'd memory.
data Executable = Executable
  { execPtr  :: {-# UNPACK #-} !(Ptr Word8)
  , execSize :: {-# UNPACK #-} !Int
  }

-- | Return the entry point pointer of an executable.
executableEntryPtr :: Executable -> Ptr Word8
executableEntryPtr = execPtr

------------------------------------------------------------------------
-- FFI
------------------------------------------------------------------------

foreign import ccall "sys/mman.h mmap"
  c_mmap :: Ptr () -> CSize -> CInt -> CInt -> CInt -> CLong -> IO (Ptr Word8)

foreign import ccall "sys/mman.h mprotect"
  c_mprotect :: Ptr Word8 -> CSize -> CInt -> IO CInt

foreign import ccall "sys/mman.h munmap"
  c_munmap :: Ptr Word8 -> CSize -> IO CInt

pageAlign :: Int -> Int
pageAlign n = (n + 4095) .&. complement 4095

------------------------------------------------------------------------
-- Loading
------------------------------------------------------------------------

-- | Load a 'CodeImage' into executable memory.  The text section bytes
-- are copied into a freshly mmap'd RX region.
loadCodeImage :: CodeImage -> IO Executable
loadCodeImage img = do
    let bytes     = codeImageBytes img
        len       = BS.length bytes
        allocSize = pageAlign (max len 1)
    buf <- c_mmap nullPtr (fromIntegral allocSize)
                  (0x1 .|. 0x2)   -- PROT_READ | PROT_WRITE
                  (0x2 .|. 0x20)  -- MAP_PRIVATE | MAP_ANONYMOUS
                  (-1) 0
    if buf == intPtrToPtr (-1)
      then ioError (userError "Harpy.CodeImage: mmap failed")
      else do
        BS.useAsCStringLen bytes $ \(src, srcLen) ->
          copyBytes buf (castPtr src) srcLen
        rc <- c_mprotect buf (fromIntegral allocSize) (0x1 .|. 0x4)
        if rc /= 0
          then do _ <- c_munmap buf (fromIntegral allocSize)
                  ioError (userError "Harpy.CodeImage: mprotect RX failed")
          else return (Executable buf allocSize)

-- | Compile code directly into executable memory.
--
-- This avoids materializing a 'CodeImage' and then copying its bytes into
-- a second mapping.  The caller owns the returned 'Executable' and must
-- release it with 'freeExecutable'.
compileExecutable :: CodeGen e s a -> e -> s -> IO (s, Either ErrMsg (a, Executable))
compileExecutable code uenv ustate = do
    (ustate', res) <- compileExecutableBuffer code uenv ustate
    case res of
      Left err -> return (ustate', Left err)
      Right (val, ptr, size) -> return (ustate', Right (val, Executable ptr size))

-- | Compile code directly into executable memory, run an action, and free
-- the executable mapping afterwards.
withCompiledExecutable :: CodeGen e s a -> e -> s -> (a -> Executable -> IO b) -> IO (s, Either ErrMsg b)
withCompiledExecutable code uenv ustate action =
    withCompiledExecutableBuffer code uenv ustate $ \val ptr size ->
      action val (Executable ptr size)

-- | Load a 'CodeImage', run an action with the executable, then free it.
withExecutable :: CodeImage -> (Executable -> IO a) -> IO a
withExecutable img = bracket (loadCodeImage img) freeExecutable

-- | Free the executable memory.
freeExecutable :: Executable -> IO ()
freeExecutable (Executable p sz) = do _ <- c_munmap p (fromIntegral sz); return ()

------------------------------------------------------------------------
-- Inspection helpers
------------------------------------------------------------------------

-- | Extract the concatenated text section bytes.
codeImageBytes :: CodeImage -> BS.ByteString
codeImageBytes img = BS.concat [sectionBytes s | s <- codeImageSections img, sectionKind s == TextSection]

-- | Total size of the text sections.
codeImageSize :: CodeImage -> Int
codeImageSize = BS.length . codeImageBytes

-- | Look up a named symbol's offset.
lookupSymbol :: String -> CodeImage -> Maybe Int
lookupSymbol name img =
    case [ciSymbolOffset s | s <- codeImageSymbols img, ciSymbolName s == name] of
      (o:_) -> Just o
      []    -> Nothing

------------------------------------------------------------------------
-- Debugging: Linux perf map
------------------------------------------------------------------------

-- | Append an entry to @\/tmp\/perf-\<pid\>.map@ for the given executable.
-- This allows @perf record@ and @perf report@ to show JIT symbol names.
-- Format per line: @\<hex-addr\> \<hex-size\> \<name\>@
writePerfMapEntry :: String -> Executable -> IO ()
writePerfMapEntry name exe = do
    CPid pid <- getProcessID
    let path = "/tmp/perf-" ++ show pid ++ ".map"
        ptr  = execPtr exe
        sz   = execSize exe
        addr = ptrToWordPtr (castPtr ptr)
        line = showHex (fromIntegral addr :: Word) ""
            ++ " " ++ showHex sz ""
            ++ " " ++ name ++ "\n"
    withFile path AppendMode $ \h -> do
      hPutStr h line
      hFlush h
