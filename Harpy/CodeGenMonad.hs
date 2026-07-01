{-# OPTIONS -cpp #-}

--------------------------------------------------------------------------
-- |
-- Module:      Harpy.CodeGenMonad
-- Copyright:   (c) 2006-20015 Martin Grabmueller and Dirk Kleeblatt
-- License:     BSD3
--
-- Maintainer:  martin@grabmueller.de
-- Stability:   provisional
-- Portability: portable (but generated code non-portable)
--
-- Monad for generating x86 machine code at runtime.
--
-- This is a combined reader-state-exception monad which handles all
-- the details of handling code buffers, emitting binary data,
-- relocation etc.
--
-- All the code generation functions in module "Harpy.X86CodeGen" live
-- in this monad and use its error reporting facilities as well as the
-- internal state maintained by the monad.
--
-- The library user can pass a user environment and user state through
-- the monad.  This state is independent from the internal state and
-- may be used by higher-level code generation libraries to maintain
-- their own state across code generation operations.
-- --------------------------------------------------------------------------

module Harpy.CodeGenMonad(
    -- * Types
          CodeGen,
          ErrMsg,
          RelocKind(..),
          Reloc,
          Label,
          FixupKind(..),
          CodeGenConfig(..),
          firstBuffer,
          defaultCodeGenConfig,
    -- * CodeImage types
          CodeImage(..),
          Section(..),
          SectionKind(..),
          CISymbol(..),
    -- * Functions
    -- ** General code generator monad operations
          failCodeGen,
    -- ** Accessing code generation internals
          getEntryPoint,
          getCodeOffset,
          getBasePtr,
          getCodeBufferList,
    -- ** Access to user state and environment
          setState,
          getState,
          getEnv,
          withEnv,
    -- ** Label management
          newLabel,
          newNamedLabel,
          setLabel,
          defineLabel,
          (@@),
          emitFixup,
          labelAddress,
          tryLabelOffset,
          emitRelocInfo,
    -- ** Code emission
          emit8,
          emit8At,
          peek8At,
          emit32,
          emit32At,
          checkBufferSize,
          ensureBufferSize,
    -- ** Executing code generation
          runCodeGen,
          runCodeGenWithConfig,
    -- ** Producing a CodeImage
          assembleCodeImage,
          assembleCodeImageWithConfig,
    -- ** Producing executable buffers
          compileExecutableBuffer,
          withCompiledExecutableBuffer,
    -- ** Calling generated functions
          callDecl,
    -- ** Interface to disassembler
          disassemble
    ) where

import Prelude hiding ((<>))
import qualified Harpy.X86Disassembler as Dis

import Control.Exception (bracket, onException)
import Control.Applicative
import Control.Monad

import Text.PrettyPrint.HughesPJ

import Numeric

import Data.Bits
import qualified Data.ByteString as BS
import Data.List
import qualified Data.Map as Map
import Foreign
import Foreign.C.Types
import System.IO

import Control.Monad.Trans

import Language.Haskell.TH.Syntax


-- | An error message produced by a code generation operation.
type ErrMsg = Doc

-- | The code generation monad, a combined reader-state-exception
-- monad.
newtype CodeGen e s a = CodeGen ((e, CodeGenEnv) -> (s, CodeGenState) -> IO ((s, CodeGenState), Either ErrMsg a))

-- | Configuration of the code generator.  There are currently two
-- configuration options.  The first is the number fo bytes to use for
-- allocating code buffers (the first as well as additional buffers
-- created in calls to 'ensureBufferSize'.  The second allows to pass
-- in a pre-allocated code buffer and its size.  When this option is
-- used, Harpy does not perform any code buffer resizing (calls to
-- 'ensureBufferSize' will be equivalent to calls to
-- 'checkBufferSize').
data CodeGenConfig = CodeGenConfig {
      codeBufferSize   :: Int,                   -- ^ Size of individual code buffer blocks.
      customCodeBuffer :: Maybe (Ptr Word8, Int) -- ^ Code buffer passed in.
    }

-- | Internal state of the code generator
data CodeGenState = CodeGenState {
      buffer        :: Ptr Word8,                    -- ^ Pointer to current code buffer.
      firstBuffer   :: Ptr Word8,                    -- ^ Pointer to first buffer (same as buffer, kept for API compat).
      bufferOfs     :: Int,                          -- ^ Current offset into buffer where next instruction will be stored.
      bufferSize    :: Int,                          -- ^ Size of current buffer.
      relocEntries  :: [Reloc],                      -- ^ List of all emitted relocation entries.
      nextLabel     :: Int,                          -- ^ Counter for generating labels.
      definedLabels :: Map.Map Int (Ptr Word8, Int, String), -- ^ Map of already defined labels.
      pendingFixups :: Map.Map Int [FixupEntry],     -- ^ Map of labels which have been referenced, but not defined.
      config        :: CodeGenConfig                 -- ^ Configuration record.
    }

data FixupEntry = FixupEntry {
      fueBuffer :: Ptr Word8,
      fueOfs    :: Int,
      fueKind   :: FixupKind
    }

-- | Kind of a fixup entry.  When a label is emitted with
-- 'defineLabel', all prior references to this label must be fixed
-- up.  This data type tells how to perform the fixup operation.
data FixupKind = Fixup8          -- ^ 8-bit relative reference
               | Fixup16         -- ^ 16-bit relative reference
               | Fixup32         -- ^ 32-bit relative reference
               | Fixup32Absolute -- ^ 32-bit absolute reference
               deriving (Show)

data CodeGenEnv = CodeGenEnv { tailContext :: Bool }
   deriving (Show)

-- | Kind of relocation, for example PC-relative
data RelocKind = RelocPCRel    -- ^ PC-relative relocation
               | RelocAbsolute -- ^ Absolute address
   deriving (Show)

-- | Relocation entry
data Reloc = Reloc { offset :: Int,
             -- ^ offset in code block which needs relocation
                     kind :: RelocKind,
             -- ^ kind of relocation
                     address :: FunPtr ()
             -- ^ target address
           }
   deriving (Show)

-- | Label
data Label = Label Int String
           deriving (Eq, Ord)

-- | What kind of data a section holds.
data SectionKind = TextSection | ReadOnlyDataSection | WritableDataSection
  deriving (Show, Eq)

-- | A section of assembled output.
data Section = Section
  { sectionKind  :: SectionKind
  , sectionBytes :: BS.ByteString
  } deriving (Show, Eq)

-- | A named symbol at an offset within the text section.
data CISymbol = CISymbol
  { ciSymbolName   :: String
  , ciSymbolOffset :: Int
  } deriving (Show, Eq)

-- | Architecture-independent assembled code.  Can be inspected,
-- serialized, or loaded into executable memory via
-- 'Harpy.CodeImage.loadCodeImage'.
data CodeImage = CodeImage
  { codeImageSections :: [Section]
  , codeImageSymbols  :: [CISymbol]
  } deriving (Show, Eq)

unCg :: CodeGen e s a -> ((e, CodeGenEnv) -> (s, CodeGenState) -> IO ((s, CodeGenState), Either ErrMsg a))
unCg (CodeGen a) = a

instance Functor (CodeGen e s) where
  fmap f m = CodeGen (\ env state -> do
                         r <- unCg m env state
                         case r of
                           (state', Left err) -> return (state', Left err)
                           (state', Right v) -> return (state', Right $ f v))

instance Applicative (CodeGen e s) where
  pure x = cgReturn x
  f <*> x = do
    f' <- f
    x' <- x
    return $ f' x'

instance Monad (CodeGen e s) where
    return = pure
    m >>= k = cgBind m k

instance MonadFail (CodeGen e s) where
    fail err = cgFail err

cgReturn :: a -> CodeGen e s a
cgReturn x = CodeGen (\_env state -> return (state, Right x))

cgFail :: String -> CodeGen e s a
cgFail err = CodeGen (\_env state -> return (state, Left (text err)))

cgBind :: CodeGen e s a -> (a -> CodeGen e s a1) -> CodeGen e s a1
cgBind m k = CodeGen (\env state ->
               do r1 <- unCg m env state
                  case r1 of
                    (state', Left err) -> return (state', Left err)
                    (state', Right v) -> unCg (k v) env state')

-- | Abort code generation with the given error message.
failCodeGen :: Doc -> CodeGen e s a
failCodeGen d = CodeGen (\_env state -> return (state, Left d))

instance MonadIO (CodeGen e s) where
  liftIO st = CodeGen (\_env state -> do { r <- st; return (state, Right r) })

emptyCodeGenState :: CodeGenState
emptyCodeGenState = CodeGenState { buffer = undefined,
                                   firstBuffer = undefined,
                                   bufferOfs = 0,
                                   bufferSize = 0,
                                   relocEntries = [],
                                   nextLabel = 0,
                                   definedLabels = Map.empty,
                                   pendingFixups = Map.empty,
                                   config = defaultCodeGenConfig}

-- | Default code generation configuration.  The code buffer size is
-- set to 4KB, and code buffer management is automatic.  This value is
-- intended to be used with record update syntax, for example:
--
-- >  runCodeGenWithConfig ... defaultCodeGenConfig{codeBufferSize = 128} ...
defaultCodeGenConfig :: CodeGenConfig
defaultCodeGenConfig = CodeGenConfig { codeBufferSize = defaultCodeBufferSize,
                                       customCodeBuffer = Nothing }

defaultCodeBufferSize :: Int
defaultCodeBufferSize = 4096

-- | Execute code generation, given a user environment and state.  The
-- result is a tuple of the resulting user state and either an error
-- message (when code generation failed) or the result of the code
-- generation.  This function runs 'runCodeGenWithConfig' with a
-- sensible default configuration.
runCodeGen :: CodeGen e s a -> e -> s -> IO (s, Either ErrMsg a)
runCodeGen cg uenv ustate =
    runCodeGenWithConfig cg uenv ustate defaultCodeGenConfig

foreign import ccall "sys/mman.h mmap"
  c_mmap :: Ptr () -> CSize -> CInt -> CInt -> CInt -> CLong -> IO (Ptr Word8)

foreign import ccall "sys/mman.h mprotect"
  c_mprotect :: Ptr Word8 -> CSize -> CInt -> IO CInt

foreign import ccall "sys/mman.h munmap"
  c_munmap :: Ptr Word8 -> CSize -> IO CInt

foreign import ccall "string.h memcpy"
  c_memcpy :: Ptr Word8 -> Ptr Word8 -> CSize -> IO (Ptr Word8)

protRead, protWrite, protExec :: CInt
protRead  = 0x1
protWrite = 0x2
protExec  = 0x4

mapPrivate, mapAnonymous :: CInt
mapPrivate   = 0x2
mapAnonymous = 0x20

mapFailed :: Ptr Word8
mapFailed = intPtrToPtr (-1)

pageAlign :: Int -> Int
pageAlign n = (n + 4095) .&. complement 4095

mmapRW :: Int -> IO (Ptr Word8)
mmapRW size = do
    let sz = fromIntegral (pageAlign size)
    p <- c_mmap nullPtr sz (protRead .|. protWrite) (mapPrivate .|. mapAnonymous) (-1) 0
    if p == mapFailed
      then ioError (userError "Harpy: mmap failed")
      else return p

mmapFree :: Ptr Word8 -> Int -> IO ()
mmapFree p size = do _ <- c_munmap p (fromIntegral (pageAlign size)); return ()

mprotectRX :: Ptr Word8 -> Int -> IO ()
mprotectRX p size = do
    rc <- c_mprotect p (fromIntegral (pageAlign size)) (protRead .|. protExec)
    if rc /= 0
      then ioError (userError "Harpy: mprotect RX failed")
      else return ()

mprotectRW :: Ptr Word8 -> Int -> IO ()
mprotectRW p size = do
    rc <- c_mprotect p (fromIntegral (pageAlign size)) (protRead .|. protWrite)
    if rc /= 0
      then ioError (userError "Harpy: mprotect RW failed")
      else return ()

-- | Like 'runCodeGen', but allows more control over the code
-- generation process.  In addition to a code generator and a user
-- environment and state, a code generation configuration must be
-- provided.  A code generation configuration allows control over the
-- allocation of code buffers, for example.
runCodeGenWithConfig :: CodeGen e s a -> e -> s -> CodeGenConfig -> IO (s, Either ErrMsg a)
runCodeGenWithConfig (CodeGen cg) uenv ustate conf =
    do (buf, sze, managed) <- case customCodeBuffer conf of
                       Nothing -> do let initSize = pageAlign (codeBufferSize conf)
                                     arr <- mmapRW initSize
                                     return (arr, initSize, True)
                       Just (buf, sze) -> return (buf, sze, False)
       let env = CodeGenEnv {tailContext = True}
       let state = emptyCodeGenState{buffer = buf,
                                     firstBuffer = buf,
                                     bufferSize = sze,
                                     config = conf}
       ((ustate', finalState), res) <- cg (uenv, env) (ustate, state)
       -- Finalize: flip buffer to RX so generated code is executable
       when managed $
         mprotectRX (firstBuffer finalState) (bufferSize finalState)
       return (ustate', res)

-- | Run code generation and capture the result as a 'CodeImage'.
-- The code is emitted into a temporary buffer (not mapped executable).
-- Use 'Harpy.CodeImage.loadCodeImage' to load the result into
-- executable memory.
assembleCodeImage :: CodeGen e s a -> e -> s -> IO (s, Either ErrMsg (a, CodeImage))
assembleCodeImage cg uenv ustate =
    assembleCodeImageWithConfig cg uenv ustate defaultCodeGenConfig

-- | Like 'assembleCodeImage', but with explicit configuration.
assembleCodeImageWithConfig :: CodeGen e s a -> e -> s -> CodeGenConfig -> IO (s, Either ErrMsg (a, CodeImage))
assembleCodeImageWithConfig (CodeGen cg) uenv ustate conf =
    do let initSize = pageAlign (max (codeBufferSize conf) 64)
       buf <- mmapRW initSize
       let env = CodeGenEnv {tailContext = True}
       let state = emptyCodeGenState{buffer = buf,
                                     firstBuffer = buf,
                                     bufferOfs = 0,
                                     bufferSize = initSize,
                                     config = conf{customCodeBuffer = Nothing}}
       ((ustate', finalState), res) <- cg (uenv, env) (ustate, state)
       case res of
         Left err -> do mmapFree (buffer finalState) (bufferSize finalState)
                        return (ustate', Left err)
         Right val -> do
           let pending = pendingFixups finalState
           if Prelude.not (Map.null pending)
             then do
               let undefs = Map.keys pending
                   msg = "undefined labels: " ++ show undefs
               mmapFree (buffer finalState) (bufferSize finalState)
               return (ustate', Left (text msg))
             else do
               let len = bufferOfs finalState
               bytes <- BS.packCStringLen (castPtr (buffer finalState), len)
               let syms = [ CISymbol name ofs
                           | (_, (_, ofs, name)) <- Map.toList (definedLabels finalState)
                           , Prelude.not (null name) ]
                   img = CodeImage
                           { codeImageSections = [Section TextSection bytes]
                           , codeImageSymbols  = syms }
               mmapFree (buffer finalState) (bufferSize finalState)
               return (ustate', Right (val, img))

-- | Compile generated code directly into an executable mmap buffer.
--
-- The returned pointer owns a whole page-aligned mapping.  Callers must
-- release it with the same mapping size via @munmap@; higher-level users
-- should prefer 'Harpy.CodeImage.compileExecutable' or
-- 'Harpy.CodeImage.withCompiledExecutable'.
compileExecutableBuffer :: CodeGen e s a -> e -> s -> IO (s, Either ErrMsg (a, Ptr Word8, Int))
compileExecutableBuffer (CodeGen cg) uenv ustate =
    do let initSize = pageAlign (max (codeBufferSize defaultCodeGenConfig) 64)
       buf <- mmapRW initSize
       let env = CodeGenEnv {tailContext = True}
       let state = emptyCodeGenState{buffer = buf,
                                     firstBuffer = buf,
                                     bufferOfs = 0,
                                     bufferSize = initSize,
                                     config = defaultCodeGenConfig{customCodeBuffer = Nothing}}
       ((ustate', finalState), res) <- cg (uenv, env) (ustate, state)
       case res of
         Left err -> do mmapFree (buffer finalState) (bufferSize finalState)
                        return (ustate', Left err)
         Right val -> do
           let pending = pendingFixups finalState
               finalBuf = firstBuffer finalState
               finalSize = bufferSize finalState
           if Prelude.not (Map.null pending)
             then do
               let undefs = Map.keys pending
                   msg = "undefined labels: " ++ show undefs
               mmapFree finalBuf finalSize
               return (ustate', Left (text msg))
             else do
               mprotectRX finalBuf finalSize `onException` mmapFree finalBuf finalSize
               return (ustate', Right (val, finalBuf, finalSize))

-- | Compile generated code into executable memory, run an action, and
-- always release the mapping afterwards.
withCompiledExecutableBuffer :: CodeGen e s a -> e -> s -> (a -> Ptr Word8 -> Int -> IO b) -> IO (s, Either ErrMsg b)
withCompiledExecutableBuffer code uenv ustate action = do
    (ustate', res) <- compileExecutableBuffer code uenv ustate
    case res of
      Left err -> return (ustate', Left err)
      Right (val, ptr, size) -> do
        out <- bracket
          (return (val, ptr, size))
          (\(_, p, sz) -> mmapFree p sz)
          (\(v, p, sz) -> action v p sz)
        return (ustate', Right out)

-- | Check whether the code buffer has room for at least the given
-- number of bytes.  This should be called by code generators
-- whenever it cannot be guaranteed that the code buffer is large
-- enough to hold all the generated code.  Lets the code generation
-- monad fail when the buffer overflows.
--
-- /Note:/ Starting with version 0.4, Harpy automatically checks for
-- buffer overflow, so you do not need to call this function anymore.
checkBufferSize :: Int -> CodeGen e s ()
checkBufferSize needed =
    do state <- getInternalState
       unless (bufferOfs state + needed <= bufferSize state)
              (failCodeGen (text "code generation buffer overflow: needed additional" <+>
                            int needed <+> text "bytes (offset =" <+>
                            int (bufferOfs state) <>
                            text ", buffer size =" <+>
                            int (bufferSize state) <> text ")"))

-- | Make sure that the code buffer has room for at least the given
-- number of bytes.  When the buffer is managed by Harpy, grows it
-- in place (mmap a larger region, copy, unmap old).  When a custom
-- buffer was provided, fails on overflow.
ensureBufferSize :: Int -> CodeGen e s ()
ensureBufferSize needed =
    do state <- getInternalState
       case customCodeBuffer (config state) of
         Nothing ->
             unless (bufferOfs state + needed <= bufferSize state)
                        (do let oldSize = bufferSize state
                                newSize = pageAlign (max (oldSize * 2) (bufferOfs state + needed))
                                oldBuf  = buffer state
                            newBuf <- liftIO $ do
                              nb <- mmapRW newSize
                              _ <- c_memcpy nb oldBuf (fromIntegral (bufferOfs state))
                              mmapFree oldBuf oldSize
                              return nb
                            st <- getInternalState
                            setInternalState st{ buffer = newBuf
                                               , firstBuffer = newBuf
                                               , bufferSize = newSize
                                               , definedLabels = Map.map (rebaseLabel oldBuf newBuf) (definedLabels st)
                                               , pendingFixups = Map.map (map (rebaseFixup oldBuf newBuf)) (pendingFixups st)
                                               })
         Just _ -> checkBufferSize needed

rebaseLabel :: Ptr Word8 -> Ptr Word8 -> (Ptr Word8, Int, String) -> (Ptr Word8, Int, String)
rebaseLabel oldBuf newBuf (buf, ofs, name)
  | buf == oldBuf = (newBuf, ofs, name)
  | otherwise     = (buf, ofs, name)

rebaseFixup :: Ptr Word8 -> Ptr Word8 -> FixupEntry -> FixupEntry
rebaseFixup oldBuf newBuf fue@(FixupEntry{fueBuffer = buf})
  | buf == oldBuf = fue{fueBuffer = newBuf}
  | otherwise     = fue

-- | Return a pointer to the beginning of the first code buffer, which
-- is normally the entry point to the generated code.
getEntryPoint :: CodeGen e s (Ptr Word8)
getEntryPoint =
    CodeGen (\ _ (ustate, state) ->
      return $ ((ustate, state), Right (firstBuffer state)))

-- | Return the current offset in the code buffer, e.g. the offset
-- at which the next instruction will be emitted.
getCodeOffset :: CodeGen e s Int
getCodeOffset =
    CodeGen (\ _ (ustate, state) ->
      return $ ((ustate, state), Right (bufferOfs state)))

-- | Set the user state to the given value.
setState :: s -> CodeGen e s ()
setState st =
    CodeGen (\ _ (_, state) ->
      return $ ((st, state), Right ()))

-- | Return the current user state.
getState :: CodeGen e s s
getState =
    CodeGen (\ _ (ustate, state) ->
      return $ ((ustate, state), Right (ustate)))

-- | Return the current user environment.
getEnv :: CodeGen e s e
getEnv =
    CodeGen (\ (uenv, _) state ->
      return $ (state, Right uenv))

-- | Set the environment to the given value and execute the given
-- code generation in this environment.
withEnv :: e -> CodeGen e s r -> CodeGen e s r
withEnv e (CodeGen cg) =
    CodeGen (\ (_, env) state ->
      cg (e, env) state)

-- | Set the user state to the given value.
setInternalState :: CodeGenState -> CodeGen e s ()
setInternalState st =
    CodeGen (\ _ (ustate, _) ->
      return $ ((ustate, st), Right ()))

-- | Return the current user state.
getInternalState :: CodeGen e s CodeGenState
getInternalState =
    CodeGen (\ _ (ustate, state) ->
      return $ ((ustate, state), Right (state)))

-- | Return the pointer to the start of the code buffer.
getBasePtr :: CodeGen e s (Ptr Word8)
getBasePtr =
    CodeGen (\ _ (ustate, state) ->
      return $ ((ustate, state), Right (buffer state)))

-- | Return a list of all code buffers and their respective size
-- (i.e., actually used space for code, not allocated size).
getCodeBufferList :: CodeGen e s [(Ptr Word8, Int)]
getCodeBufferList = do st <- getInternalState
                       return [(buffer st, bufferOfs st)]

-- | Generate a new label to be used with the label operations
-- 'emitFixup' and 'defineLabel'.
newLabel :: CodeGen e s Label
newLabel =
    do state <- getInternalState
       let lab = nextLabel state
       setInternalState state{nextLabel = lab + 1}
       return (Label lab "")

-- | Generate a new label to be used with the label operations
-- 'emitFixup' and 'defineLabel'.  The given name is used for
-- diagnostic purposes, and will appear in the disassembly.
newNamedLabel :: String -> CodeGen e s Label
newNamedLabel name =
    do state <- getInternalState
       let lab = nextLabel state
       setInternalState state{nextLabel = lab + 1}
       return (Label lab name)

-- | Generate a new label and define it at once
setLabel :: CodeGen e s Label
setLabel =
    do l <- newLabel
       defineLabel l
       return l

-- | Emit a relocation entry for the given offset, relocation kind
-- and target address.
emitRelocInfo :: Int -> RelocKind -> FunPtr a -> CodeGen e s ()
emitRelocInfo ofs knd addr =
    do state <- getInternalState
       setInternalState state{relocEntries =
                              Reloc{offset = ofs,
                                    kind = knd,
                                    address = castFunPtr addr} :
                              (relocEntries state)}

-- | Emit a byte value to the code buffer.
emit8 :: Word8 -> CodeGen e s ()
emit8 op =
    CodeGen (\ _ (ustate, state) ->
      do let buf = buffer state
             ptr = bufferOfs state
         pokeByteOff buf ptr op
         return $ ((ustate, state{bufferOfs = ptr + 1}), Right ()))

-- | Store a byte value at the given offset into the code buffer.
emit8At :: Int -> Word8 -> CodeGen e s ()
emit8At pos op =
    CodeGen (\ _ (ustate, state) ->
      do let buf = buffer state
         pokeByteOff buf pos op
         return $ ((ustate, state), Right ()))

-- | Return the byte value at the given offset in the code buffer.
peek8At :: Int -> CodeGen e s Word8
peek8At pos =
    CodeGen (\ _ (ustate, state) ->
      do let buf = buffer state
         b <- peekByteOff buf pos
         return $ ((ustate, state), Right b))

-- | Like 'emit8', but for a 32-bit value.
emit32 :: Word32 -> CodeGen e s ()
emit32 op =
    CodeGen (\ _ (ustate, state) ->
      do let buf = buffer state
             ptr = bufferOfs state
         pokeByteOff buf ptr op
         return $ ((ustate, state{bufferOfs = ptr + 4}), Right ()))

-- | Like 'emit8At', but for a 32-bit value.
emit32At :: Int -> Word32 -> CodeGen e s ()
emit32At pos op =
    CodeGen (\ _ (ustate, state) ->
      do let buf = buffer state
         pokeByteOff buf pos op
         return $ ((ustate, state), Right ()))

-- | Emit a label at the current offset in the code buffer.  All
-- references to the label will be relocated to this offset.
defineLabel :: Label -> CodeGen e s ()
defineLabel (Label lab name) =
    do state <- getInternalState
       case Map.lookup lab (definedLabels state) of
         Just _ -> failCodeGen $ text "duplicate definition of label" <+>
                     int lab
         _ -> return ()
       case Map.lookup lab (pendingFixups state) of
         Just fixups -> do mapM_ (performFixup (buffer state) (bufferOfs state)) fixups
                           setInternalState state{pendingFixups = Map.delete lab (pendingFixups state)}
         Nothing -> return ()
       state1 <- getInternalState
       setInternalState state1{definedLabels = Map.insert lab (buffer state1, bufferOfs state1, name) (definedLabels state1)}

performFixup :: Ptr Word8 -> Int -> FixupEntry -> CodeGen e s ()
performFixup labBuf labOfs (FixupEntry{fueBuffer = buf, fueOfs = ofs, fueKind = knd}) =
    do let diff = (labBuf `plusPtr` labOfs) `minusPtr` (buf `plusPtr` ofs)
       liftIO $ case knd of
                  Fixup8  -> pokeByteOff buf ofs (fromIntegral diff - 1 :: Word8)
                  Fixup16 -> pokeByteOff buf ofs (fromIntegral diff - 2 :: Word16)
                  Fixup32 -> pokeByteOff buf ofs (fromIntegral diff - 4 :: Word32)
                  Fixup32Absolute -> pokeByteOff buf ofs (fromIntegral (ptrToWordPtr (labBuf `plusPtr` labOfs)) :: Word32)
       return ()


-- | This operator gives neat syntax for defining labels.  When @l@ is a label, the code
--
-- > l @@ mov eax ebx
--
-- associates the label l with the following @mov@ instruction.
(@@) :: Label -> CodeGen e s a -> CodeGen e s a
(@@) lab gen = do defineLabel lab
                  gen

-- | Emit a fixup entry for the given label at the current offset in
-- the code buffer (unless the label is already defined).
-- The instruction at this offset will
-- be patched to target the address associated with this label when
-- it is defined later.
emitFixup :: Label -> Int -> FixupKind -> CodeGen e s ()
emitFixup (Label lab _) ofs knd =
    do state <- getInternalState
       let base = buffer state
           ptr = bufferOfs state
           fue = FixupEntry{fueBuffer = base,
                            fueOfs = ptr + ofs,
                            fueKind = knd}
       case Map.lookup lab (definedLabels state) of
         Just (labBuf, labOfs, _) -> performFixup labBuf labOfs fue
         Nothing -> setInternalState state{pendingFixups = Map.insertWith (++) lab [fue] (pendingFixups state)}

-- | Return the address of a label, fail if the label is not yet defined.
labelAddress :: Label -> CodeGen e s (Ptr a)
labelAddress (Label lab name) = do
  state <- getInternalState
  case Map.lookup lab (definedLabels state) of
    Just (labBuf, labOfs, _) -> return $ plusPtr labBuf labOfs
    Nothing -> fail $ "Label " ++ show lab ++ "(" ++ name ++ ") not yet defined"

-- | Try to get the buffer offset of a label, if it is already defined.
-- Returns @Nothing@ for forward (not yet defined) labels.
tryLabelOffset :: Label -> CodeGen e s (Maybe Int)
tryLabelOffset (Label lab _) = do
  state <- getInternalState
  case Map.lookup lab (definedLabels state) of
    Just (_, labOfs, _) -> return (Just labOfs)
    Nothing -> return Nothing

-- | Disassemble all code buffers.  The result is a list of
-- disassembled instructions which can be converted to strings using
-- the 'Dis.showIntel' or 'Dis.showAtt' functions from module
-- "Harpy.X86Disassembler".
disassemble :: CodeGen e s [Dis.Instruction]
disassemble = do
  s <- getInternalState
  r <- liftIO $ Dis.disassembleBlock (buffer s) (bufferOfs s)
  case r of
    Left err -> cgFail $ show err
    Right instrs -> insertLabels instrs
 where insertLabels :: [Dis.Instruction] -> CodeGen e s [Dis.Instruction]
       insertLabels = liftM concat . mapM ins
       ins :: Dis.Instruction -> CodeGen e s [Dis.Instruction]
       ins i@(Dis.BadInstruction{}) = return [i]
       ins i@(Dis.PseudoInstruction{}) = return [i]
       ins i@(Dis.Instruction{Dis.address = addr}) =
           do state <- getInternalState
              let allLabs = Map.toList (definedLabels state)
                  labs = filter (\ (_, (buf, ofs, _)) -> fromIntegral (ptrToWordPtr (buf `plusPtr` ofs)) == addr) allLabs
                  createLabel (l, (buf, ofs, name)) = Dis.PseudoInstruction addr
                                                        (case name of
                                                           "" ->
                                                               "label " ++ show l ++
                                                                " [" ++
                                                                hex32 (fromIntegral (ptrToWordPtr (buf `plusPtr` ofs))) ++
                                                                "]"
                                                           _ -> name ++ ": [" ++
                                                                  hex32 (fromIntegral (ptrToWordPtr (buf `plusPtr` ofs))) ++
                                                                  "]")
              return $ fmap createLabel labs ++ [i]
       hex32 :: Int -> String
       hex32 i =
              let w :: Word32
                  w = fromIntegral i
                  s = showHex w ""
              in take (8 - length s) (repeat '0') ++ s

#ifndef __HADDOCK__

callDecl :: String -> Q Type -> Q [Dec]
callDecl ns qt =  do
    t0 <- qt
    let (tvars, cxt, t) = case t0 of
                         ForallT vs c t' -> (vs, c, t')
                         _ -> ([], [], t0)
    let name = mkName ns
    let funptr = AppT (ConT $ mkName "FunPtr") t
    let ioresult = t -- addIO t
    let ty = AppT (AppT ArrowT funptr) ioresult
    dynName <- newName "conv"
    let dyn = ForeignD $ ImportF CCall Safe "dynamic" dynName $ ForallT tvars cxt ty
    vs <- mkArgs t
    cbody <- [| CodeGen (\_env (ustate, state) ->
                        do let code = firstBuffer state
                               sz   = bufferSize state
                               managed = case customCodeBuffer (config state) of
                                           Nothing -> True
                                           Just _  -> False
                           when managed $ mprotectRX code sz
                           res <- liftIO $ $(do
                                             c <- newName "c"
                                             cast <- [|castPtrToFunPtr|]
                                             let f = AppE (VarE dynName)
                                                          (AppE cast
                                                                (VarE c))
                                             return $ LamE [VarP c] $ foldl AppE f $ map VarE vs
                                            ) code
                           when managed $ mprotectRW code sz
                           return $ ((ustate, state), Right res))|]
    let call = ValD (VarP name) (NormalB $ LamE (map VarP vs) cbody) []
    return [ dyn, call ]

mkArgs (AppT (AppT ArrowT _from) to) = do
  v  <- newName "v"
  vs <- mkArgs to
  return $ v : vs
mkArgs _ = return []

addIO (AppT t@(AppT ArrowT _from) to) = AppT t $ addIO to
addIO t = AppT (ConT $ mkName "IO") t

#else

-- | Declare a stub function to call the code buffer. Arguments are the name
-- of the generated function, and the type the code buffer is supposed to have.
-- The type argument can be given using the [t| ... |] notation of Template Haskell.
-- Allowed types are the legal types for FFI functions.
callDecl :: String -> Q Type -> Q [Dec]

#endif
