module Data.BoardStateTypes
  ( Message(..)
  , BoardState
  , StateElem
  ) where

import Prelude

import Ansi.Codes (EscapeCode(..))
import Data.Maybe (Maybe)
import Data.Player (Player, Square)
import Data.Position (Position)

--- BoardState Types ---

type BoardState =
  { history :: Array (Position -> Maybe Player) -- | holding state transition functions
  , nextTurn :: Player -- | which Player would have next move
  , reset :: Maybe (Array (Position -> Maybe Player))
  }

-- | Message is the output from the Cell component
data Message
  = IsClicked Position
  | HasWinner Square
  | IsReturned Int

instance Show Message where
  show (IsClicked p) = "Clicked on " <> show p
  show (HasWinner s) = "Winner is " <> show s
  show (IsReturned i) = "Revert state by " <> show i

type StateElem = Position -> Square

