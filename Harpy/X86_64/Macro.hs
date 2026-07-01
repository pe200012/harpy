{-# LANGUAGE DataKinds, ScopedTypeVariables, TypeApplications #-}
module Harpy.X86_64.Macro (
    -- * SysV64 ABI helpers
      prologue
    , epilogue
    , withFrame
    -- * Argument registers (SysV64)
    , argRegs
    -- * Callee-saved registers (SysV64)
    , calleeSaved
    -- * Stack operations
    , pushAll, popAll
    , alignStack
    -- * Call helpers
    , callPtr
    ) where

import Prelude hiding (and, or, not)
import Data.Bits ((.&.), complement)
import Data.Int (Int64)
import Data.Word
import Foreign.Ptr (FunPtr, castFunPtrToPtr, ptrToWordPtr)

import Harpy.CodeGenMonad (CodeGen)
import Harpy.X86_64

-- | SysV64 integer argument registers in order.
argRegs :: [Reg 'W64]
argRegs = [rdi, rsi, rdx, rcx, r8, r9]

-- | SysV64 callee-saved registers.
calleeSaved :: [Reg 'W64]
calleeSaved = [rbx, rbp, r12, r13, r14, r15]

-- | Push a list of 64-bit registers.
pushAll :: [Reg 'W64] -> CodeGen e s ()
pushAll = mapM_ push

-- | Pop a list of 64-bit registers (in reverse order).
popAll :: [Reg 'W64] -> CodeGen e s ()
popAll = mapM_ pop . reverse

-- | Emit a standard SysV64 prologue: push rbp; mov rbp, rsp; sub rsp, N.
-- N is rounded up to 16-byte alignment. Pass 0 for a leaf that needs
-- no stack frame beyond the saved rbp.
prologue :: Word32 -> CodeGen e s ()
prologue frameSize = do
  push rbp
  mov (op rbp) (op rsp)
  let aligned = (frameSize + 15) .&. complement 15
  if aligned > 0
    then sub (op rsp) (imm (fromIntegral aligned))
    else return ()

-- | Emit a standard SysV64 epilogue: mov rsp, rbp; pop rbp; ret.
epilogue :: CodeGen e s ()
epilogue = do
  mov (op rsp) (op rbp)
  pop rbp
  ret

-- | Bracket a function body with prologue/epilogue.
-- @withFrame frameSize body@ emits prologue, runs body, emits epilogue.
withFrame :: Word32 -> CodeGen e s a -> CodeGen e s a
withFrame n body = do
  prologue n
  r <- body
  epilogue
  return r

-- | Align RSP to 16 bytes (call-site alignment for SysV64).
-- Useful before calling into C when the push count is odd.
alignStack :: CodeGen e s ()
alignStack = Harpy.X86_64.and (op rsp) (imm (-16))

-- | Load a function pointer into RAX and call it.
-- Clobbers RAX. Arguments must already be in the SysV64 argument registers.
callPtr :: FunPtr a -> CodeGen e s ()
callPtr fp = do
  let w = fromIntegral (ptrToWordPtr (castFunPtrToPtr fp)) :: Int64
  mov (op rax) (imm w)
  call rax
