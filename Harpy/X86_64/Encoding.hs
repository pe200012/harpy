--------------------------------------------------------------------------
-- |
-- Module:      Harpy.X86_64.Encoding
-- License:     BSD3
--
-- Measured byte encodings for the x86-64 assembler.  An 'Encoding'
-- records its size, a compositional 'Builder' representation for image
-- backends, and the concrete bytes used by the current direct emitter.
--------------------------------------------------------------------------

module Harpy.X86_64.Encoding
  ( Encoding
  , encodingSize
  , encodingBuilder
  , encodingBytes
  , byte
  , bytes
  , word16LE
  , word32LE
  , word64LE
  , emitEncoding
  ) where

import Data.Bits (shiftR)
import qualified Data.ByteString.Builder as Builder
import Data.Word (Word8, Word16, Word32, Word64)

import Harpy.CodeGenMonad (CodeGen, emit8, ensureBufferSize)

-- | A small measured machine-code fragment.
data Encoding = Encoding
  { encodingSize    :: !Int
  , encodingBuilder :: !Builder.Builder
  , encodingBytes   :: [Word8]
  }

instance Semigroup Encoding where
  Encoding n b xs <> Encoding m c ys =
    Encoding (n + m) (b <> c) (xs <> ys)

instance Monoid Encoding where
  mempty = Encoding 0 mempty []

-- | Encode one byte.
byte :: Word8 -> Encoding
byte w = Encoding 1 (Builder.word8 w) [w]

-- | Encode a fixed byte sequence.
bytes :: [Word8] -> Encoding
bytes = foldMap byte

-- | Encode a little-endian 16-bit word.
word16LE :: Word16 -> Encoding
word16LE w = Encoding 2 (Builder.word16LE w)
  [ fromIntegral w
  , fromIntegral (w `shiftR` 8)
  ]

-- | Encode a little-endian 32-bit word.
word32LE :: Word32 -> Encoding
word32LE w = Encoding 4 (Builder.word32LE w)
  [ fromIntegral w
  , fromIntegral (w `shiftR` 8)
  , fromIntegral (w `shiftR` 16)
  , fromIntegral (w `shiftR` 24)
  ]

-- | Encode a little-endian 64-bit word.
word64LE :: Word64 -> Encoding
word64LE w = Encoding 8 (Builder.word64LE w)
  [ fromIntegral w
  , fromIntegral (w `shiftR` 8)
  , fromIntegral (w `shiftR` 16)
  , fromIntegral (w `shiftR` 24)
  , fromIntegral (w `shiftR` 32)
  , fromIntegral (w `shiftR` 40)
  , fromIntegral (w `shiftR` 48)
  , fromIntegral (w `shiftR` 56)
  ]

-- | Emit an encoding through the existing direct code-generation backend.
emitEncoding :: Encoding -> CodeGen e s ()
emitEncoding encoding = do
    ensureBufferSize (encodingSize encoding)
    mapM_ emit8 (encodingBytes encoding)
