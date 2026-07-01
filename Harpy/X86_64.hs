{-# LANGUAGE DataKinds, GADTs, KindSignatures, ScopedTypeVariables,
             TypeApplications, RankNTypes, MultiParamTypeClasses,
             FlexibleInstances, FlexibleContexts, StandaloneDeriving #-}
--------------------------------------------------------------------------
-- |
-- Module:      Harpy.X86_64
-- License:     BSD3
--
-- Width-indexed x86-64 assembler with compile-time operand-size checking.
-- Replaces the old Harpy.X86Assembler / Harpy.X86CodeGen modules.
--------------------------------------------------------------------------

module Harpy.X86_64 (
    -- * Width and register types
      Width(..), SWidth(..), IsWidth(..)
    -- | 'Reg', 'XMM', 'Mem' and 'Operand' are exported abstractly: build
    -- them only through the named registers and the smart constructors below,
    -- so no ill-formed register code or operand can be constructed.
    , Reg, XMM
    -- * Named registers (64-bit)
    , rax, rcx, rdx, rbx, rsp, rbp, rsi, rdi
    , r8, r9, r10, r11, r12, r13, r14, r15
    -- * Named registers (32-bit)
    , eax, ecx, edx, ebx, esp, ebp, esi, edi
    , r8d, r9d, r10d, r11d, r12d, r13d, r14d, r15d
    -- * Named registers (16-bit)
    , ax, cx, dx, bx, sp, bp, si, di
    -- * Named registers (8-bit)
    , al, cl, dl, bl
    -- * Named XMM registers
    , xmm0, xmm1, xmm2, xmm3, xmm4, xmm5, xmm6, xmm7
    , xmm8, xmm9, xmm10, xmm11, xmm12, xmm13, xmm14, xmm15
    -- * Memory operands
    , Mem, addr, base, index, baseIndex, disp
    -- * Operand type
    , Operand, op, mem, imm
    -- * Scale
    , Scale(..)
    -- * ISA database types
    , InsnForm(..), InsnDesc(..)
    -- * ISA table entries
    , iADD, iOR, iADC, iSBB, iAND, iSUB, iXOR, iCMP
    , iINC, iDEC, iNEG, iNOT, iIDIV, iMUL
    , iSHL, iSHR, iSAR, iROL, iROR
    , iIMUL, iPUSH, iPOP
    -- * Operand-width constraints
    , LeaWidth, ImulWidth
    -- * Instructions
    , mov, add, sub, xor, and, or, cmp, test
    , lea
    , push, pop
    , ret, retN
    , call, jmp
    , jcc, Cond(..)
    , je, jne, jl, jge, jle, jg, jb, jae, jbe, ja, jo, jno, js, jns
    , jmpLabel
    , nop
    , inc, dec, neg, not
    , imul, idiv
    , shl, shr, sar
    , pxorX, movdquLoad, movdquXmm, pmulld, paddd, psrldq, movdFromXmm
    , cdq, cqo
    , syscall
    , breakpoint
    -- * Labels
    , Label, newLabel, newNamedLabel, setLabel, defineLabel, (@@)
    ) where

import Prelude hiding (and, or, not)
import qualified Prelude
import Data.Bits ((.&.), (.|.), shiftL, shiftR, testBit)
import Data.Int
import Data.Word

import Harpy.CodeGenMonad
    ( CodeGen, Label
    , emit8, emit32
    , ensureBufferSize, getCodeOffset
    , newLabel, newNamedLabel, setLabel, defineLabel, (@@)
    , emitFixup, FixupKind(..)
    , tryLabelOffset
    , failCodeGen
    )
import Text.PrettyPrint (text)
import qualified Harpy.X86_64.Encoding as Enc

------------------------------------------------------------------------
-- Width
------------------------------------------------------------------------

data Width = W8 | W16 | W32 | W64

data SWidth (w :: Width) where
  SW8  :: SWidth 'W8
  SW16 :: SWidth 'W16
  SW32 :: SWidth 'W32
  SW64 :: SWidth 'W64

deriving instance Show (SWidth w)

class IsWidth (w :: Width) where
  swidth :: SWidth w

instance IsWidth 'W8  where swidth = SW8
instance IsWidth 'W16 where swidth = SW16
instance IsWidth 'W32 where swidth = SW32
instance IsWidth 'W64 where swidth = SW64

------------------------------------------------------------------------
-- Registers
------------------------------------------------------------------------

data Reg (w :: Width) where
  Reg :: Word8 -> Reg w

deriving instance Show (Reg w)
deriving instance Eq (Reg w)

-- | XMM register used by the small SSE/SSE4.1 instruction subset.
data XMM = XMM Word8
  deriving (Show, Eq)

regCode :: Reg w -> Word8
regCode (Reg c) = c .&. 0x07

regExt :: Reg w -> Bool
regExt (Reg c) = testBit c 3

xmmCode :: XMM -> Word8
xmmCode (XMM c) = c .&. 0x07

xmmExt :: XMM -> Bool
xmmExt (XMM c) = testBit c 3

-- 64-bit
rax, rcx, rdx, rbx, rsp, rbp, rsi, rdi :: Reg 'W64
r8, r9, r10, r11, r12, r13, r14, r15   :: Reg 'W64
rax = Reg 0;  rcx = Reg 1;  rdx = Reg 2;  rbx = Reg 3
rsp = Reg 4;  rbp = Reg 5;  rsi = Reg 6;  rdi = Reg 7
r8  = Reg 8;  r9  = Reg 9;  r10 = Reg 10; r11 = Reg 11
r12 = Reg 12; r13 = Reg 13; r14 = Reg 14; r15 = Reg 15

-- 32-bit
eax, ecx, edx, ebx, esp, ebp, esi, edi :: Reg 'W32
r8d, r9d, r10d, r11d, r12d, r13d, r14d, r15d :: Reg 'W32
eax = Reg 0; ecx = Reg 1; edx = Reg 2; ebx = Reg 3
esp = Reg 4; ebp = Reg 5; esi = Reg 6; edi = Reg 7
r8d = Reg 8; r9d = Reg 9; r10d = Reg 10; r11d = Reg 11
r12d = Reg 12; r13d = Reg 13; r14d = Reg 14; r15d = Reg 15

-- 16-bit
ax, cx, dx, bx, sp, bp, si, di :: Reg 'W16
ax = Reg 0; cx = Reg 1; dx = Reg 2; bx = Reg 3
sp = Reg 4; bp = Reg 5; si = Reg 6; di = Reg 7

-- 8-bit (low only; no AH/BH/CH/DH)
al, cl, dl, bl :: Reg 'W8
al = Reg 0; cl = Reg 1; dl = Reg 2; bl = Reg 3

-- XMM registers
xmm0, xmm1, xmm2, xmm3, xmm4, xmm5, xmm6, xmm7 :: XMM
xmm8, xmm9, xmm10, xmm11, xmm12, xmm13, xmm14, xmm15 :: XMM
xmm0 = XMM 0; xmm1 = XMM 1; xmm2 = XMM 2; xmm3 = XMM 3
xmm4 = XMM 4; xmm5 = XMM 5; xmm6 = XMM 6; xmm7 = XMM 7
xmm8 = XMM 8; xmm9 = XMM 9; xmm10 = XMM 10; xmm11 = XMM 11
xmm12 = XMM 12; xmm13 = XMM 13; xmm14 = XMM 14; xmm15 = XMM 15

------------------------------------------------------------------------
-- Memory operands
------------------------------------------------------------------------

data Scale = S1 | S2 | S4 | S8 deriving (Show, Eq)

scaleVal :: Scale -> Word8
scaleVal S1 = 0; scaleVal S2 = 1; scaleVal S4 = 2; scaleVal S8 = 3

data Mem (w :: Width) = Mem
  { memBase  :: Maybe (Reg 'W64)
  , memIndex :: Maybe (Reg 'W64, Scale)
  , memDisp  :: Int32
  }

addr :: Int32 -> Mem w
addr d = Mem Nothing Nothing d

base :: Reg 'W64 -> Mem w
base r = Mem (Just r) Nothing 0

index :: Reg 'W64 -> Scale -> Int32 -> Mem w
index r s d = Mem Nothing (Just (r, s)) d

-- | @baseIndex b i s d@ builds the operand @[b + i*s + d]@.  @i@ must not be
-- 'rsp' (it encodes \"no index\" in a SIB byte); this is rejected at emit time.
baseIndex :: Reg 'W64 -> Reg 'W64 -> Scale -> Int32 -> Mem w
baseIndex b i s d = Mem (Just b) (Just (i, s)) d

disp :: Reg 'W64 -> Int32 -> Mem w
disp r d = Mem (Just r) Nothing d

------------------------------------------------------------------------
-- Operands
------------------------------------------------------------------------

data Operand (w :: Width) where
  RegOp :: Reg w -> Operand w
  MemOp :: Mem w -> Operand w
  ImmOp :: Int64 -> Operand w

deriving instance Show (Operand w)

op :: Reg w -> Operand w
op = RegOp

mem :: Mem w -> Operand w
mem = MemOp

imm :: Int64 -> Operand w
imm = ImmOp

instance Show (Mem w) where
  show (Mem b i d) =
    let bs = maybe "" (\(Reg c) -> "r" ++ Prelude.show c) b
        is = maybe "" (\(Reg c, s) -> "r" ++ Prelude.show c ++ "*" ++ Prelude.show (1 `shiftL` fromIntegral (scaleVal s) :: Int)) i
        ds = if d == 0 && (b /= Nothing || i /= Nothing) then "" else Prelude.show d
    in "[" ++ bs ++ (if bs /= "" && is /= "" then "+" else "") ++ is
           ++ (if (bs /= "" || is /= "") && ds /= "" then "+" else "") ++ ds ++ "]"

------------------------------------------------------------------------
-- Encoding helpers
------------------------------------------------------------------------

maxInsnBytes :: Int
maxInsnBytes = 15

-- REX prefix byte construction
emitRex :: Word8 -> CodeGen e s ()
emitRex bits = emit8 (0x40 .|. bits)

rexW, rexR, rexX, rexB :: Word8
rexW = 0x08; rexR = 0x04; rexX = 0x02; rexB = 0x01

needsRex :: IsWidth w => SWidth w -> Bool
needsRex SW64 = True
needsRex _    = False

-- Emit REX for register-only instruction (reg in ModRM.reg, rm in ModRM.rm)
emitRexRR :: forall w e s. IsWidth w => Reg w -> Reg w -> CodeGen e s ()
emitRexRR reg rm = do
  let bits = (if needsRex (swidth @w) then rexW else 0)
         .|. (if regExt reg then rexR else 0)
         .|. (if regExt rm  then rexB else 0)
  case swidth @w of
    SW16 -> emit8 0x66
    _    -> return ()
  if bits /= 0 then emitRex bits else return ()

-- REX for reg + memory operand
emitRexRM :: forall w e s. IsWidth w => Reg w -> Mem w -> CodeGen e s ()
emitRexRM reg m = do
  let bBit = case memBase m of
               Just r | regExt r -> rexB
               _ -> 0
      xBit = case memIndex m of
               Just (r, _) | regExt r -> rexX
               _ -> 0
      bits = (if needsRex (swidth @w) then rexW else 0)
         .|. (if regExt reg then rexR else 0)
         .|. xBit .|. bBit
  case swidth @w of
    SW16 -> emit8 0x66
    _    -> return ()
  if bits /= 0 then emitRex bits else return ()

-- REX for single register in ModRM.rm (e.g. INC, DEC, NOT, NEG, PUSH, etc.)
emitRexR :: forall w e s. IsWidth w => Reg w -> CodeGen e s ()
emitRexR rm = do
  let bits = (if needsRex (swidth @w) then rexW else 0)
         .|. (if regExt rm then rexB else 0)
  case swidth @w of
    SW16 -> emit8 0x66
    _    -> return ()
  if bits /= 0 then emitRex bits else return ()

-- REX for single register in opcode +rd (e.g. PUSH 50+rd, POP 58+rd, MOV B8+rd)
emitRexOp :: forall w e s. IsWidth w => Reg w -> CodeGen e s ()
emitRexOp = emitRexR @w

-- REX for XMM register instructions. The R bit encodes ModRM.reg and
-- the B bit encodes ModRM.r/m.
emitRexXMMRR :: XMM -> XMM -> CodeGen e s ()
emitRexXMMRR reg rm = do
  let bits = (if xmmExt reg then rexR else 0)
         .|. (if xmmExt rm then rexB else 0)
  if bits /= 0 then emitRex bits else return ()

emitRexXMMRM :: XMM -> Mem w -> CodeGen e s ()
emitRexXMMRM reg m = do
  let bBit = case memBase m of
               Just r | regExt r -> rexB
               _ -> 0
      xBit = case memIndex m of
               Just (r, _) | regExt r -> rexX
               _ -> 0
      bits = (if xmmExt reg then rexR else 0) .|. xBit .|. bBit
  if bits /= 0 then emitRex bits else return ()

emitRexXMMGpr :: XMM -> Reg w -> CodeGen e s ()
emitRexXMMGpr reg rm = do
  let bits = (if xmmExt reg then rexR else 0)
         .|. (if regExt rm then rexB else 0)
  if bits /= 0 then emitRex bits else return ()

rexEncoding :: Word8 -> Enc.Encoding
rexEncoding bits =
  if bits == 0 then mempty else Enc.byte (0x40 .|. bits)

rexEncodingXMMRR :: XMM -> XMM -> Enc.Encoding
rexEncodingXMMRR reg rm =
  rexEncoding $
       (if xmmExt reg then rexR else 0)
  .|. (if xmmExt rm then rexB else 0)

rexEncodingXMMRM :: XMM -> Mem w -> Enc.Encoding
rexEncodingXMMRM reg m =
  let bBit = case memBase m of
               Just r | regExt r -> rexB
               _ -> 0
      xBit = case memIndex m of
               Just (r, _) | regExt r -> rexX
               _ -> 0
  in rexEncoding $
       (if xmmExt reg then rexR else 0) .|. xBit .|. bBit

rexEncodingXMMGpr :: XMM -> Reg w -> Enc.Encoding
rexEncodingXMMGpr reg rm =
  rexEncoding $
       (if xmmExt reg then rexR else 0)
  .|. (if regExt rm then rexB else 0)

xmmRR :: [Word8] -> [Word8] -> XMM -> XMM -> Enc.Encoding
xmmRR prefixes opcode dst src =
     Enc.bytes prefixes
  <> rexEncodingXMMRR dst src
  <> Enc.bytes opcode
  <> Enc.byte (modRM 3 (xmmCode dst) (xmmCode src))

-- ModRM byte: mod(2) reg(3) rm(3)
modRM :: Word8 -> Word8 -> Word8 -> Word8
modRM m r rm = (m `shiftL` 6) .|. ((r .&. 7) `shiftL` 3) .|. (rm .&. 7)

-- Emit ModRM for two registers (mod=11)
emitModRMrr :: Reg w -> Reg w -> CodeGen e s ()
emitModRMrr reg rm = emit8 (modRM 3 (regCode reg) (regCode rm))

-- Emit ModRM + optional SIB + displacement for a memory operand
emitModRMmem :: Word8 -> Mem w -> CodeGen e s ()
emitModRMmem regBits m = checkIndex >> case (memBase m, memIndex m, memDisp m) of
  -- [disp32] (no base, no index) — use SIB form: mod=00, rm=100(SIB), base=101(none), index=100(none)
  (Nothing, Nothing, d) -> do
    emit8 (modRM 0 regBits 4)
    emit8 (modRM 0 4 5)  -- SIB: scale=0, index=RSP(none), base=RBP(disp32)
    emit32 (fromIntegral d)

  -- [base + disp]
  (Just br, Nothing, d) -> do
    let bc = regCode br
    if bc == 4 -- RSP/R12 needs SIB
      then do
        let md = modBits d (bc == 5)
        emit8 (modRM md regBits 4) -- rm=100 => SIB
        emit8 (modRM 0 4 bc)       -- SIB: scale=0, index=RSP(none), base=br
        emitDisp md d
      else if bc == 5 && d == 0 -- RBP/R13 with 0 disp needs mod=01 + 0x00
        then do
          emit8 (modRM 1 regBits bc)
          emit8 0
        else do
          let md = modBits d False
          emit8 (modRM md regBits bc)
          emitDisp md d

  -- [index*scale + disp32]
  (Nothing, Just (ir, sc), d) -> do
    emit8 (modRM 0 regBits 4) -- rm=100 => SIB
    emit8 (modRM (scaleVal sc) (regCode ir) 5) -- base=101(none)
    emit32 (fromIntegral d)

  -- [base + index*scale + disp]
  (Just br, Just (ir, sc), d) -> do
    let bc = regCode br
        md = modBits d (bc == 5)
    emit8 (modRM md regBits 4)
    emit8 (modRM (scaleVal sc) (regCode ir) bc)
    emitDisp md d
  where
    -- SIB index 0b100 means "no index", so RSP can never be a SIB index.
    checkIndex :: CodeGen e s ()
    checkIndex = case memIndex m of
      Just (ir, _) | regCode ir == 4 && Prelude.not (regExt ir) ->
        failCodeGen (text "RSP cannot be used as a SIB index register")
      _ -> return ()
    modBits :: Int32 -> Bool -> Word8
    modBits d forceDisp
      | d == 0 && Prelude.not forceDisp = 0
      | isImm8 d                        = 1
      | otherwise                       = 2
    emitDisp :: Word8 -> Int32 -> CodeGen e s ()
    emitDisp 0 _ = return ()
    emitDisp 1 d = emit8 (fromIntegral d)
    emitDisp _ d = emit32 (fromIntegral d)

emit16 :: Word16 -> CodeGen e s ()
emit16 n = do
  emit8 (fromIntegral n)
  emit8 (fromIntegral (n `shiftR` 8))

emitImmWidth :: SWidth w -> Int64 -> CodeGen e s ()
emitImmWidth SW8  i = emit8 (fromIntegral i)
emitImmWidth SW16 i = emit16 (fromIntegral i)
emitImmWidth _    i = emit32 (fromIntegral i)

isW8 :: SWidth w -> Bool
isW8 SW8 = True
isW8 _   = False

aluRmRegOpcode :: SWidth w -> Word8 -> Word8
aluRmRegOpcode w grp = (grp `shiftL` 3) .|. if isW8 w then 0 else 1

aluRegRmOpcode :: SWidth w -> Word8 -> Word8
aluRegRmOpcode w grp = (grp `shiftL` 3) .|. if isW8 w then 2 else 3

aluAccImmOpcode :: SWidth w -> Word8 -> Word8
aluAccImmOpcode w grp = (grp `shiftL` 3) .|. if isW8 w then 4 else 5

aluImmOpcode :: SWidth w -> Int64 -> Word8
aluImmOpcode SW8  _ = 0x80
aluImmOpcode _    i = if isImm8' i then 0x83 else 0x81

emitAluImmediate :: SWidth w -> Int64 -> CodeGen e s ()
emitAluImmediate SW8  i = emit8 (fromIntegral i)
emitAluImmediate SW16 i
  | isImm8' i  = emit8 (fromIntegral i)
  | otherwise = emit16 (fromIntegral i)
emitAluImmediate _ i
  | isImm8' i  = emit8 (fromIntegral i)
  | otherwise = emit32 (fromIntegral i)

unaryOpcode :: SWidth w -> Word8 -> Word8
unaryOpcode SW8 0xFF = 0xFE
unaryOpcode SW8 0xF7 = 0xF6
unaryOpcode _   opc  = opc

isImm8 :: Int32 -> Bool
isImm8 n = n >= -128 && n <= 127

isImm8' :: Int64 -> Bool
isImm8' n = n >= -128 && n <= 127

isImm32' :: Int64 -> Bool
isImm32' n = n >= fromIntegral (minBound :: Int32) && n <= fromIntegral (maxBound :: Int32)

-- | Reject an immediate that does not fit the operand width before it is
-- silently truncated by the encoder.  A 64-bit operand takes a
-- sign-extended @imm32@ (the sole exception, @mov r64,imm64@, does its own
-- emission and never calls this), so its accepted range is @imm32@.
checkImm :: SWidth w -> Int64 -> CodeGen e s ()
checkImm w i = case w of
  SW8  | i >= -128        && i <= 255        -> return ()
  SW16 | i >= -32768      && i <= 65535      -> return ()
  SW32 | i >= -2147483648 && i <= 4294967295 -> return ()
  SW64 | isImm32' i                          -> return ()
  _ -> failCodeGen $ text $
         "immediate " ++ Prelude.show i ++ " out of range for " ++
         Prelude.show w ++ " operand"

-- | Range-check the immediate carried by a source operand (a no-op for
-- register/memory sources).
checkSrcImm :: SWidth w -> Operand w -> CodeGen e s ()
checkSrcImm w (ImmOp i) = checkImm w i
checkSrcImm _ _         = return ()

------------------------------------------------------------------------
-- Declarative instruction database
------------------------------------------------------------------------

-- | Encoding form for an instruction. Each constructor captures a family
-- of instructions that share the same encoding pattern, parameterized
-- by opcode bytes or digit extensions.
data InsnForm
  = FormALU Word8
    -- ^ ALU group 0-7 (add/or/adc/sbb/and/sub/xor/cmp).
    -- Handles reg/reg, reg/imm, reg/mem, mem/reg, mem/imm variants
    -- with automatic imm8 sign-extension and EAX/RAX shortcuts.
  | FormUnary Word8 Word8
    -- ^ @opcode /digit@ for single-operand r/m instructions.
    -- First byte is the opcode (0xFF, 0xF7, etc.), second is the
    -- ModRM.reg extension digit.
  | FormShift Word8
    -- ^ Shift/rotate group. The digit selects the operation:
    -- SHL=4 SHR=5 SAR=7 ROL=0 ROR=1 RCL=2 RCR=3.
    -- Handles reg/1, reg/imm8, and reg/CL forms.
  | FormTwoByteReg Word8
    -- ^ Two-byte opcode @0F xx /r@ for reg,r/m instructions (IMUL, MOVZX, etc.).
  | FormOpRd Word8
    -- ^ Opcode+rd encoding for single-register instructions (PUSH, POP).
    -- Only used for 64-bit registers (no REX.W, just REX.B if extended).
  deriving (Show, Eq)

-- | Instruction descriptor — one row per instruction in the ISA table.
data InsnDesc = InsnDesc
  { insnName :: String
  , insnForm :: InsnForm
  } deriving (Show, Eq)

------------------------------------------------------------------------
-- Generic emitters driven by InsnForm
------------------------------------------------------------------------

-- ALU: two-operand, same-width, all addressing modes
emitFormALU :: forall w e s. IsWidth w => Word8 -> Operand w -> Operand w -> CodeGen e s ()
emitFormALU grp dst src = ensureBufferSize maxInsnBytes >> checkSrcImm (swidth @w) src >> case (dst, src) of
  (RegOp rd, RegOp rs) -> do
    emitRexRR @w rs rd
    emit8 (aluRmRegOpcode (swidth @w) grp)
    emitModRMrr rs rd
  (RegOp rd, ImmOp i)
    | isW8 (swidth @w) && regCode rd == 0 && Prelude.not (regExt rd) -> do
        emitRexR @w rd
        emit8 (aluAccImmOpcode (swidth @w) grp)
        emit8 (fromIntegral i)
    | regCode rd == 0 && Prelude.not (regExt rd) && Prelude.not (isImm8' i) -> do
        emitRexR @w rd
        emit8 (aluAccImmOpcode (swidth @w) grp)
        emitImmWidth (swidth @w) i
    | otherwise -> do
        emitRexR @w rd
        emit8 (aluImmOpcode (swidth @w) i)
        emit8 (modRM 3 grp (regCode rd))
        emitAluImmediate (swidth @w) i
  (RegOp rd, MemOp m) -> do
    emitRexRM @w rd m
    emit8 (aluRegRmOpcode (swidth @w) grp)
    emitModRMmem (regCode rd) m
  (MemOp m, RegOp rs) -> do
    emitRexRM @w rs m
    emit8 (aluRmRegOpcode (swidth @w) grp)
    emitModRMmem (regCode rs) m
  (MemOp m, ImmOp i)
    | otherwise -> do
        emitRexRM @w (Reg @w 0) m
        emit8 (aluImmOpcode (swidth @w) i)
        emitModRMmem grp m
        emitAluImmediate (swidth @w) i
  _ -> failCodeGen (text "invalid ALU operand combination (an immediate cannot be a destination, and two memory operands are not allowed)")

-- Unary: single r/m operand, opcode + /digit
emitFormUnary :: forall w e s. IsWidth w => Word8 -> Word8 -> Operand w -> CodeGen e s ()
emitFormUnary opc digit o = ensureBufferSize maxInsnBytes >> case o of
  RegOp r -> do
    emitRexR @w r
    emit8 (unaryOpcode (swidth @w) opc)
    emit8 (modRM 3 digit (regCode r))
  MemOp m -> do
    emitRexRM @w (Reg @w 0) m
    emit8 (unaryOpcode (swidth @w) opc)
    emitModRMmem digit m
  _ -> failCodeGen (text "invalid unary operand (an immediate has no r/m form)")

-- Shift: dst, src (imm or CL)
emitFormShift :: forall w e s. IsWidth w => Word8 -> Operand w -> Operand w -> CodeGen e s ()
emitFormShift digit dst src = ensureBufferSize maxInsnBytes >> case (dst, src) of
  (RegOp rd, ImmOp 1) -> do
    emitRexR @w rd
    emit8 (if isW8 (swidth @w) then 0xD0 else 0xD1)
    emit8 (modRM 3 digit (regCode rd))
  (RegOp rd, ImmOp i) -> do
    checkImm SW8 i   -- shift count is an imm8
    emitRexR @w rd
    emit8 (if isW8 (swidth @w) then 0xC0 else 0xC1)
    emit8 (modRM 3 digit (regCode rd))
    emit8 (fromIntegral i)
  (RegOp rd, RegOp (Reg 1)) -> do
    emitRexR @w rd
    emit8 (if isW8 (swidth @w) then 0xD2 else 0xD3)
    emit8 (modRM 3 digit (regCode rd))
  _ -> failCodeGen (text "invalid shift operand combination (variable shifts must use CL as the count)")

-- Two-byte reg,r/m: 0F xx /r
emitFormTwoByteReg :: forall w e s. IsWidth w => Word8 -> Reg w -> Operand w -> CodeGen e s ()
emitFormTwoByteReg opc2 rd src = ensureBufferSize maxInsnBytes >> case src of
  RegOp rs -> do
    emitRexRR @w rd rs
    emit8 0x0F
    emit8 opc2
    emitModRMrr rd rs
  MemOp m -> do
    emitRexRM @w rd m
    emit8 0x0F
    emit8 opc2
    emitModRMmem (regCode rd) m
  _ -> failCodeGen (text "invalid two-byte reg operand (an immediate source is not supported)")

-- Opcode+rd: PUSH/POP style, 64-bit only, no REX.W
emitFormOpRd :: Word8 -> Reg 'W64 -> CodeGen e s ()
emitFormOpRd opcBase r = do
  ensureBufferSize maxInsnBytes
  if regExt r then emitRex rexB else return ()
  emit8 (opcBase + regCode r)

------------------------------------------------------------------------
-- Instruction table (declarative ISA database)
------------------------------------------------------------------------

-- ALU family
iADD, iOR, iADC, iSBB, iAND, iSUB, iXOR, iCMP :: InsnDesc
iADD = InsnDesc "add" (FormALU 0)
iOR  = InsnDesc "or"  (FormALU 1)
iADC = InsnDesc "adc" (FormALU 2)
iSBB = InsnDesc "sbb" (FormALU 3)
iAND = InsnDesc "and" (FormALU 4)
iSUB = InsnDesc "sub" (FormALU 5)
iXOR = InsnDesc "xor" (FormALU 6)
iCMP = InsnDesc "cmp" (FormALU 7)

-- Unary r/m family
iINC, iDEC, iNEG, iNOT, iIDIV, iMUL :: InsnDesc
iINC  = InsnDesc "inc"  (FormUnary 0xFF 0)
iDEC  = InsnDesc "dec"  (FormUnary 0xFF 1)
iNEG  = InsnDesc "neg"  (FormUnary 0xF7 3)
iNOT  = InsnDesc "not"  (FormUnary 0xF7 2)
iIDIV = InsnDesc "idiv" (FormUnary 0xF7 7)
iMUL  = InsnDesc "mul"  (FormUnary 0xF7 4)

-- Shift family
iSHL, iSHR, iSAR, iROL, iROR :: InsnDesc
iSHL = InsnDesc "shl" (FormShift 4)
iSHR = InsnDesc "shr" (FormShift 5)
iSAR = InsnDesc "sar" (FormShift 7)
iROL = InsnDesc "rol" (FormShift 0)
iROR = InsnDesc "ror" (FormShift 1)

-- Two-byte reg,r/m
iIMUL :: InsnDesc
iIMUL = InsnDesc "imul" (FormTwoByteReg 0xAF)

-- Opcode+rd
iPUSH, iPOP :: InsnDesc
iPUSH = InsnDesc "push" (FormOpRd 0x50)
iPOP  = InsnDesc "pop"  (FormOpRd 0x58)

------------------------------------------------------------------------
-- Public instruction API (thin wrappers over the table)
------------------------------------------------------------------------

-- ALU
add, sub, xor, and, or, cmp :: IsWidth w => Operand w -> Operand w -> CodeGen e s ()
add = emitFormALU 0
or  = emitFormALU 1
and = emitFormALU 4
sub = emitFormALU 5
xor = emitFormALU 6
cmp = emitFormALU 7

-- TEST has a unique encoding (not in the ALU family)
test :: forall w e s. IsWidth w => Operand w -> Operand w -> CodeGen e s ()
test dst src = ensureBufferSize maxInsnBytes >> checkSrcImm (swidth @w) src >> case (dst, src) of
  (RegOp rd, RegOp rs) -> do
    emitRexRR @w rs rd
    emit8 (if isW8 (swidth @w) then 0x84 else 0x85)
    emitModRMrr rs rd
  (RegOp rd, ImmOp i)
    | regCode rd == 0 && Prelude.not (regExt rd) -> do
        emitRexR @w rd
        emit8 (if isW8 (swidth @w) then 0xA8 else 0xA9)
        emitImmWidth (swidth @w) i
    | otherwise -> do
        emitRexR @w rd
        emit8 (if isW8 (swidth @w) then 0xF6 else 0xF7)
        emit8 (modRM 3 0 (regCode rd))
        emitImmWidth (swidth @w) i
  (MemOp m, RegOp rs) -> do
    emitRexRM @w rs m
    emit8 (if isW8 (swidth @w) then 0x84 else 0x85)
    emitModRMmem (regCode rs) m
  _ -> failCodeGen (text "invalid TEST operand combination")

-- MOV has multiple encoding forms (not table-driven yet)
mov :: forall w e s. IsWidth w => Operand w -> Operand w -> CodeGen e s ()
mov dst src = ensureBufferSize maxInsnBytes >> case (dst, src) of
  (RegOp rd, RegOp rs) -> do
    emitRexRR @w rs rd
    emit8 (if isW8 (swidth @w) then 0x88 else 0x89)
    emitModRMrr rs rd
  (RegOp rd, ImmOp i) -> case swidth @w of
    SW64
      | isImm32' i -> do
          emitRexR @w rd
          emit8 0xC7
          emit8 (modRM 3 0 (regCode rd))
          emit32 (fromIntegral i)
      | otherwise -> do
          emitRexOp @w rd
          emit8 (0xB8 + regCode rd)
          emit32 (fromIntegral i)
          emit32 (fromIntegral (i `shiftR` 32))
    -- SW64 above accepts a full imm64 (movabs); narrower widths must fit.
    SW32 -> do
      checkImm SW32 i
      emitRexOp @w rd
      emit8 (0xB8 + regCode rd)
      emit32 (fromIntegral i)
    SW16 -> do
      checkImm SW16 i
      emitRexOp @w rd
      emit8 (0xB8 + regCode rd)
      emit8 (fromIntegral i)
      emit8 (fromIntegral (i `shiftR` 8))
    SW8 -> do
      checkImm SW8 i
      emitRexOp @w rd
      emit8 (0xB0 + regCode rd)
      emit8 (fromIntegral i)
  (RegOp rd, MemOp m) -> do
    emitRexRM @w rd m
    emit8 (if isW8 (swidth @w) then 0x8A else 0x8B)
    emitModRMmem (regCode rd) m
  (MemOp m, RegOp rs) -> do
    emitRexRM @w rs m
    emit8 (if isW8 (swidth @w) then 0x88 else 0x89)
    emitModRMmem (regCode rs) m
  (MemOp m, ImmOp i) -> do
    checkImm (swidth @w) i
    emitRexRM @w (Reg @w 0) m
    emit8 (if isW8 (swidth @w) then 0xC6 else 0xC7)
    emitModRMmem 0 m
    emitImmWidth (swidth @w) i
  _ -> failCodeGen (text "invalid MOV operand combination")

-- | Widths with a LEA form.  LEA has no 8-bit encoding, so @lea al ...@ is
-- rejected at compile time instead of silently assembling a 32-bit LEA.
class LeaWidth (w :: Width)
instance LeaWidth 'W16
instance LeaWidth 'W32
instance LeaWidth 'W64

-- | Widths with a two-operand IMUL (0F AF) form.  There is no 8-bit form.
class ImulWidth (w :: Width)
instance ImulWidth 'W16
instance ImulWidth 'W32
instance ImulWidth 'W64

lea :: forall w e s. (IsWidth w, LeaWidth w) => Reg w -> Mem w -> CodeGen e s ()
lea rd m = do
  ensureBufferSize maxInsnBytes
  emitRexRM @w rd m
  emit8 0x8D
  emitModRMmem (regCode rd) m

-- Unary r/m
inc, dec, neg, not :: IsWidth w => Operand w -> CodeGen e s ()
inc = emitFormUnary 0xFF 0
dec = emitFormUnary 0xFF 1
neg = emitFormUnary 0xF7 3
not = emitFormUnary 0xF7 2

idiv :: IsWidth w => Operand w -> CodeGen e s ()
idiv = emitFormUnary 0xF7 7

-- Two-byte reg,r/m
imul :: (IsWidth w, ImulWidth w) => Reg w -> Operand w -> CodeGen e s ()
imul = emitFormTwoByteReg 0xAF

-- SSE/SSE4.1 vector integer subset.
pxorX :: XMM -> XMM -> CodeGen e s ()
pxorX dst src =
  Enc.emitEncoding (xmmRR [0x66] [0x0F, 0xEF] dst src)

movdquLoad :: XMM -> Mem w -> CodeGen e s ()
movdquLoad dst src = do
  ensureBufferSize maxInsnBytes
  Enc.emitEncoding $
       Enc.byte 0xF3
    <> rexEncodingXMMRM dst src
    <> Enc.bytes [0x0F, 0x6F]
  emitModRMmem (xmmCode dst) src

movdquXmm :: XMM -> XMM -> CodeGen e s ()
movdquXmm dst src =
  Enc.emitEncoding (xmmRR [0xF3] [0x0F, 0x6F] dst src)

pmulld :: XMM -> XMM -> CodeGen e s ()
pmulld dst src =
  Enc.emitEncoding (xmmRR [0x66] [0x0F, 0x38, 0x40] dst src)

paddd :: XMM -> XMM -> CodeGen e s ()
paddd dst src =
  Enc.emitEncoding (xmmRR [0x66] [0x0F, 0xFE] dst src)

psrldq :: XMM -> Word8 -> CodeGen e s ()
psrldq dst amount =
  Enc.emitEncoding $
       Enc.byte 0x66
    <> rexEncoding (if xmmExt dst then rexB else 0)
    <> Enc.bytes [0x0F, 0x73]
    <> Enc.byte (modRM 3 3 (xmmCode dst))
    <> Enc.byte amount

movdFromXmm :: Reg 'W32 -> XMM -> CodeGen e s ()
movdFromXmm dst src =
  Enc.emitEncoding $
       Enc.byte 0x66
    <> rexEncodingXMMGpr src dst
    <> Enc.bytes [0x0F, 0x7E]
    <> Enc.byte (modRM 3 (xmmCode src) (regCode dst))

-- Shifts
shl, shr, sar :: IsWidth w => Operand w -> Operand w -> CodeGen e s ()
shl = emitFormShift 4
shr = emitFormShift 5
sar = emitFormShift 7

-- Push/Pop (64-bit only)
push, pop :: Reg 'W64 -> CodeGen e s ()
push = emitFormOpRd 0x50
pop  = emitFormOpRd 0x58

-- CALL/JMP via register (FF /2 and FF /4, no REX.W needed)
call, jmp :: Reg 'W64 -> CodeGen e s ()
call r = do
  ensureBufferSize maxInsnBytes
  if regExt r then emitRex rexB else return ()
  emit8 0xFF
  emit8 (modRM 3 2 (regCode r))
jmp r = do
  ensureBufferSize maxInsnBytes
  if regExt r then emitRex rexB else return ()
  emit8 0xFF
  emit8 (modRM 3 4 (regCode r))

-- Fixed-byte instructions
ret :: CodeGen e s ()
ret = ensureBufferSize 1 >> emit8 0xC3

retN :: Word16 -> CodeGen e s ()
retN n = do
  ensureBufferSize 3
  emit8 0xC2
  emit8 (fromIntegral n)
  emit8 (fromIntegral (n `shiftR` 8))

nop :: CodeGen e s ()
nop = ensureBufferSize 1 >> emit8 0x90

cdq :: CodeGen e s ()
cdq = ensureBufferSize 1 >> emit8 0x99

cqo :: CodeGen e s ()
cqo = ensureBufferSize 2 >> emitRex rexW >> emit8 0x99

syscall :: CodeGen e s ()
syscall = ensureBufferSize 2 >> emit8 0x0F >> emit8 0x05

breakpoint :: CodeGen e s ()
breakpoint = ensureBufferSize 1 >> emit8 0xCC

-- Conditional jumps
data Cond
  = O | NO | B | AE | E | NE | BE | A
  | S | NS | P | NP | L | GE | LE | G
  deriving (Show, Eq)

condCode :: Cond -> Word8
condCode O  = 0x0; condCode NO = 0x1; condCode B  = 0x2; condCode AE = 0x3
condCode E  = 0x4; condCode NE = 0x5; condCode BE = 0x6; condCode A  = 0x7
condCode S  = 0x8; condCode NS = 0x9; condCode P  = 0xA; condCode NP = 0xB
condCode L  = 0xC; condCode GE = 0xD; condCode LE = 0xE; condCode G  = 0xF

-- | Conditional jump with automatic short/near selection.
-- For backward jumps (label already defined), emits the 2-byte short
-- form (7x rel8) when the displacement fits in ±127 bytes.
-- For forward jumps or out-of-range backward jumps, emits the 6-byte
-- near form (0F 8x rel32).
jcc :: Cond -> Label -> CodeGen e s ()
jcc cc lbl = do
  ensureBufferSize 6
  mlabOfs <- tryLabelOffset lbl
  curOfs  <- getCodeOffset
  case mlabOfs of
    Just labOfs ->
      let shortDisp = labOfs - (curOfs + 2)  -- rel8 is relative to end of 2-byte insn
      in if shortDisp >= -128 && shortDisp <= 127
        then do
          emit8 (0x70 + condCode cc)
          emit8 (fromIntegral shortDisp)
        else do
          emit8 0x0F
          emit8 (0x80 + condCode cc)
          emitFixup lbl 0 Fixup32
          emit32 0
    Nothing -> do
      emit8 0x0F
      emit8 (0x80 + condCode cc)
      emitFixup lbl 0 Fixup32
      emit32 0

je, jne, jl, jge, jle, jg, jb, jae, jbe, ja :: Label -> CodeGen e s ()
jo, jno, js, jns :: Label -> CodeGen e s ()
je  = jcc E;  jne = jcc NE; jl  = jcc L;  jge = jcc GE
jle = jcc LE; jg  = jcc G;  jb  = jcc B;  jae = jcc AE
jbe = jcc BE; ja  = jcc A;  jo  = jcc O;  jno = jcc NO
js  = jcc S;  jns = jcc NS

-- | Unconditional jump to label with short/near relaxation.
jmpLabel :: Label -> CodeGen e s ()
jmpLabel lbl = do
  ensureBufferSize 5
  mlabOfs <- tryLabelOffset lbl
  curOfs  <- getCodeOffset
  case mlabOfs of
    Just labOfs ->
      let shortDisp = labOfs - (curOfs + 2)
      in if shortDisp >= -128 && shortDisp <= 127
        then do
          emit8 0xEB
          emit8 (fromIntegral shortDisp)
        else do
          emit8 0xE9
          emitFixup lbl 0 Fixup32
          emit32 0
    Nothing -> do
      emit8 0xE9
      emitFixup lbl 0 Fixup32
      emit32 0
