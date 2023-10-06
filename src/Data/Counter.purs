module Data.Counter
  ( Counter
  , init
  , next
  , rewind
  ) where

import Prelude
import Data.Maybe (Maybe(..))

newtype Counter = MkCounter Int

derive instance Eq Counter 

instance Show Counter where
  show (MkCounter c) = "Counter: " <> show c

 

--| API make use of the Counter for movies on the Board 
rewind :: Int -> Counter -> Maybe Counter
rewind ix (MkCounter c)
  | ix < c = Just <<< MkCounter $ c - ix
  | otherwise = Nothing

init :: Counter
init = MkCounter 0

next :: Counter -> Counter
next (MkCounter x) = MkCounter $ x + 1
