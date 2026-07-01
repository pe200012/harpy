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
import qualified Harpy.Internal.ExecutableMemory as ExecMem

import Control.Exception (bracket, onException)
import qualified Data.ByteString as BS
import Data.Word
import Foreign
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
-- Loading
------------------------------------------------------------------------

-- | Load a 'CodeImage' into executable memory.  The text section bytes
-- are copied into a freshly mmap'd RX region.
loadCodeImage :: CodeImage -> IO Executable
loadCodeImage img = do
    let bytes     = codeImageBytes img
        len       = BS.length bytes
    mapping <- ExecMem.allocate len
    (do BS.useAsCStringLen bytes $ \(src, srcLen) ->
          copyBytes (ExecMem.mappingPtr mapping) (castPtr src) srcLen
        ExecMem.protect ExecMem.ReadExecute mapping
        return (Executable (ExecMem.mappingPtr mapping) (ExecMem.mappingSize mapping)))
      `onException` ExecMem.free mapping

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
freeExecutable (Executable p sz) = ExecMem.free (ExecMem.Mapping p sz)

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
