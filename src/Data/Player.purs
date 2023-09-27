module Data.Player 
  ( Player(..)
  , Square(..)  
  , init
  , next
  , rewind 
  ) where

import Prelude 

import Data.Array as Array
import Data.Maybe (Maybe)

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow) 
import Data.Eq.Generic (genericEq)

-- | Board state to present to Game State
data Player = X | O

derive instance genericPlayer :: Generic Player _

instance showPlayer :: Show Player where
  show = genericShow

instance eqPlayer :: Eq Player where
  eq = genericEq

type Square = Maybe Player

--- Player API ---

init :: Player 
init = X

next :: Player -> Player 
next X = O
next _ = X 

rewind :: Int -> Player -> Player   
rewind i p 
  = Array.foldr (\f p -> f p) p 
  $ Array.replicate i next
