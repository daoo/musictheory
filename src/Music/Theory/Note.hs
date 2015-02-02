{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Music.Theory.Note
  ( Note
  , c0

  , (.+)
  , (+.)
  , (.-.)

  , Interval
  , Interval12
  , fromInterval12
  , unsafeInterval12
  , safeInterval12

  , perfectUnison
  , minor2
  , major2
  , minor3
  , major3
  , perfect4
  , tritone
  , perfect5
  , minor6
  , major6
  , minor7
  , major7
  , perfectOctave
  ) where

import Test.QuickCheck

-- |A note (in semitones).
newtype Note = Note { mkNote :: Int }
  deriving (Show, Eq, Ord)

instance Arbitrary Note where
  arbitrary = fmap Note arbitrary
  shrink = map Note . shrink . mkNote

c0 :: Note
c0 = Note 0

-- | Add an interval to a note.
--
-- prop> a .+ 0 == a
-- prop> 0 +. a == a
-- prop> a .+ b == b +. a
(.+) :: Note -> Interval -> Note
(+.) :: Interval -> Note -> Note
n .+ i = Note (mkNote     n + mkInterval i)
i +. n = Note (mkInterval i + mkNote     n)

(.-.) :: Note -> Note -> Interval
n .-. m = Interval (mkNote n - mkNote m)

-- |Distance (in semitones) between two notes.
newtype Interval = Interval { mkInterval :: Int }
  deriving (Eq, Ord, Enum, Num, Real, Integral, Show)

newtype Interval12 = Interval12 { mkInterval12 :: Int }
  deriving (Eq, Ord)

fromInterval12 :: Interval12 -> Int
fromInterval12 = mkInterval12

safeInterval12 :: Interval -> Interval12
safeInterval12 i = Interval12 $ mkInterval i `mod` 12

unsafeInterval12 :: Interval -> Interval12
unsafeInterval12 = Interval12 . mkInterval

-- |Diatonic Intervals.
perfectUnison, minor2, major2, minor3, major3, perfect4, tritone, perfect5,
  minor6, major6, minor7, major7, perfectOctave :: Interval
perfectUnison = Interval 0
minor2        = Interval 1
major2        = Interval 2
minor3        = Interval 3
major3        = Interval 4
perfect4      = Interval 5
tritone       = Interval 6
perfect5      = Interval 7
minor6        = Interval 8
major6        = Interval 9
minor7        = Interval 10
major7        = Interval 11
perfectOctave = Interval 12
