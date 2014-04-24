{-# LANGUAGE BangPatterns, MagicHash #-}
module Music.Theory.Scale
  ( Scale
  , emptyScale
  , fullScale
  , hasOffset
  , setOffset
  , (#+)
  , fromOffsets
  , toOffsets
  , lower
  , raise
  ) where

import Data.Bits
import Data.List
import Data.Word
import GHC.Exts
import Music.Theory.Note
import Test.QuickCheck (Arbitrary, arbitrary, choose)

-- |Representation for a octave (12 notes) repeating scale.
--
-- The scale is represented as a 16 bit word, where each bit starting from the
-- least significant bit marks if the note with the same index as the bit is
-- included in the scale or not. The upper 4 bits are unused.
newtype Scale = Scale { bitField :: Word16 }
  deriving (Eq, Show)

-- |The scale containing no notes.
emptyScale :: Scale
emptyScale = Scale 0

-- |The scale containing all notes.
fullScale :: Scale
fullScale = Scale 4095

instance Arbitrary Scale where
  arbitrary = Scale `fmap` choose (bitField emptyScale, bitField fullScale)

-- |Decides if the note at a specific scale offset is included or not.
--
-- > forall i. hasOffset emptyScale i == False
-- > forall i. hasOffset fullScale i == True
hasOffset :: Scale -> ScaleOffset -> Bool
hasOffset s = unsafeTest (bitField s) . fromScaleOffset

-- |Set a note at a specific offset to be included.
setOffset :: Scale -> ScaleOffset -> Scale
setOffset (Scale s) = Scale . unsafeSet s . fromScaleOffset

-- |Infix version of 'setOffset' with Int as type.
(#+) :: Scale -> Word8 -> Scale
Scale s #+ i = Scale (unsafeSet s (fromIntegral i))

-- |Merge a list of scale offsets into a scale.
fromOffsets :: [ScaleOffset] -> Scale
fromOffsets = foldl' setOffset emptyScale

toOffsets :: Scale -> [ScaleOffset]
toOffsets (Scale bits) = enumFilter 0 11 (unsafeTest bits) unsafeScaleOffset

-- |Lower the a note in the scale one half step.
lower :: Int -> Scale -> Scale
lower i (Scale bits) = Scale $ unsafeMove (-1) i bits

-- |Raise the a note in the scale one half step.
raise :: Int -> Scale -> Scale
raise i (Scale bits) = Scale $ unsafeMove 1 i bits

unsafeTest :: Word16 -> Int -> Bool
unsafeTest a (I# i) = isTrue# (andI# (uncheckedIShiftRL# a' i) 1#)
  where
    !(I# a') = fromIntegral a

unsafeSet :: Word16 -> Int -> Word16
unsafeSet a i = unsafeShiftL 1 i .|. a

unsafeClear :: Word16 -> Int -> Word16
unsafeClear a i = a .&. complement (unsafeShiftL 1 i)

unsafeMove :: Int -> Int -> Word16 -> Word16
unsafeMove d i a = unsafeSet (unsafeClear a i) (i + d)

enumFilter :: (Ord a, Enum a) => a -> a -> (a -> Bool) -> (a -> b) -> [b]
enumFilter a b f g = go a
  where
    go !i | i > b     = []
          | f i       = g i : go (succ i)
          | otherwise = go (succ i)
