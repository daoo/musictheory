{-# LANGUAGE BangPatterns #-}
module Music.Theory.Scale
  ( Scale
  , emptyScale
  , hasOffset
  , setOffset
  , (#+)
  , fromOffsets
  , toOffsets
  , lower
  , raise
  ) where

import Control.Exception
import Data.Bits
import Data.Word
import Music.Theory.Note
import Test.QuickCheck (Arbitrary, arbitrary, choose)

-- |Type for a octave repeating scale.
newtype Scale = Scale { bitField :: Word }
  deriving (Eq, Show)

emptyScale :: Scale
emptyScale = Scale 0

instance Arbitrary Scale where
  arbitrary = Scale `fmap` choose (1, 4095) -- 4095 = 2^12-1

unsafeTest :: Word -> Int -> Bool
unsafeTest a i = assert (i >= 0 && i < 64) $
  (unsafeShiftR a i .&. 1) == 1

unsafeSet :: Word -> Int -> Word
unsafeSet a i = assert (i >= 0 && i < 64) $
  unsafeShiftL 1 i .|. a

unsafeClear :: Word -> Int -> Word
unsafeClear a i = assert (i >= 0 && i < 64) $
  a .&. complement (unsafeShiftL 1 i)

hasOffset :: Scale -> ScaleOffset -> Bool
hasOffset s = unsafeTest (bitField s) . fromScaleOffset

setOffset :: Scale -> Int -> Scale
setOffset (Scale s) i = Scale (unsafeSet s i)

(#+) :: Scale -> Int -> Scale
(#+) = setOffset

fromOffsets :: [ScaleOffset] -> Scale
fromOffsets = Scale . go 0
  where
    go !acc []     = acc
    go !acc (x:xs) = go (unsafeSet acc (fromScaleOffset x)) xs

toOffsets :: Scale -> [ScaleOffset]
toOffsets (Scale bits) = enumFilter 0 11 (unsafeTest bits) unsafeScaleOffset

moveBit :: Int -> Int -> Word -> Word
moveBit d i a = unsafeSet (unsafeClear a i) (i + d)

lower, raise :: Int -> Scale -> Scale
lower i (Scale bits) = Scale $ moveBit (-1) i bits
raise i (Scale bits) = Scale $ moveBit 1 i bits

enumFilter :: (Ord a, Enum a) => a -> a -> (a -> Bool) -> (a -> b) -> [b]
enumFilter a b f g = go a
  where
    go !i | i > b     = []
          | f i       = g i : go (succ i)
          | otherwise = go (succ i)
