module Music.Theory.Scale.Rooted
  ( Rooted(..)
  , hasNote
  ) where

import Music.Theory.Note
import Music.Theory.Scale

data Rooted = Rooted
  { root  :: !Note
  , scale :: !Scale
  } deriving Show

hasNote :: Note -> Rooted -> Bool
hasNote n (Rooted r s) = hasOffset s (scaled $ n `off` r)
