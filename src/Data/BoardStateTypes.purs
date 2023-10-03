module Data.BoardStateTypes
  ( Message(..)
  , BoardState
  , StateElem
  ) where

import Prelude

import Data.Player (Player, Square)
import Data.Position (Position)
import Data.Maybe (Maybe)

--- BoardState Types ---

type BoardState =
  { history :: Array (Position -> Maybe Player) -- | holding state transition functions
  , nextTurn :: Player -- | which Player would have next move
  }

-- | Message is the output from the Cell component
data Message
  = IsClicked Position -- StateElem
  | HasWinner BoardState Square
  | IsReturned Int

instance Show Message where
  show (IsClicked p) = "Clicked on " <> show p
  show (HasWinner _ s) = "Winner is " <> show s
  show (IsReturned i) = "Revert state by " <> show i

type StateElem = Position -> Square

