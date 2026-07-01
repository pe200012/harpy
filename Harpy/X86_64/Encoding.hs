--------------------------------------------------------------------------
-- |
-- Module:      Harpy.X86_64.Encoding
-- License:     BSD3
--
-- Measured byte encodings for the x86-64 assembler.  An 'Encoding'
-- records its size and the concrete little-endian bytes.
--------------------------------------------------------------------------

module Harpy.X86_64.Encoding
  ( Encoding
  , encodingSize
  , encodingBytes
  , byte
  , bytes
  , word16LE
  , word32LE
  , word64LE
  , emitEncoding
  ) where

import Data.Bits (shiftR)
import Data.Word (Word8, Word16, Word32, Word64)

import Harpy.CodeGenMonad (CodeGen, emit8, ensureBufferSize)

-- | A small measured machine-code fragment.  'encodingSize' is redundant
-- with @length . encodingBytes@ but kept so buffer sizing is O(1).
data Encoding = Encoding
  { encodingSize  :: !Int
  , encodingBytes :: [Word8]
  }

instance Semigroup Encoding where
  Encoding n xs <> Encoding m ys = Encoding (n + m) (xs <> ys)

instance Monoid Encoding where
  mempty = Encoding 0 []

-- | Encode one byte.
byte :: Word8 -> Encoding
byte w = Encoding 1 [w]

-- | Encode a fixed byte sequence.
bytes :: [Word8] -> Encoding
bytes = foldMap byte

-- | Encode a little-endian 16-bit word.
word16LE :: Word16 -> Encoding
word16LE w = Encoding 2
  [ fromIntegral w
  , fromIntegral (w `shiftR` 8)
  ]

-- | Encode a little-endian 32-bit word.
word32LE :: Word32 -> Encoding
word32LE w = Encoding 4
  [ fromIntegral w
  , fromIntegral (w `shiftR` 8)
  , fromIntegral (w `shiftR` 16)
  , fromIntegral (w `shiftR` 24)
  ]

-- | Encode a little-endian 64-bit word.
word64LE :: Word64 -> Encoding
word64LE w = Encoding 8
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
