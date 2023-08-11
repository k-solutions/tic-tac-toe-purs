module Types  
  ( mkPosition
  , emptySquare
  , emptyStateElem
  , sizeArray
  , Player(..)
  , Square
  , Message(..)
  , StateElem
  , Position
  , GameState(..)
  , MoveCount(..)
  ) where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEArray
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)

--- Data  Types ---

data Player = X | O

derive instance genericPlayer :: Generic Player _

instance showPlayer :: Show Player where
  show = genericShow

instance eqPlayer :: Eq Player where
  eq = genericEq

type Square = Maybe Player

type StateElem = Position -> Square

newtype Position = MkPosition
  { row :: Int
  , col :: Int
  }

instance Eq Position where
  eq (MkPosition { row: r1, col: c1 }) (MkPosition { row: r2, col: c2 }) = r1 == r2 && c1 == c2 

instance Ord Position where
  compare (MkPosition {row: r1, col: c1}) (MkPosition {row: r2, col: c2}) 
    = case compare r1 r2 of
          EQ -> compare c1 c2
          LT -> LT
          GT -> GT

newtype MoveCount = MkMoveCount Int

type GameState =
  { history :: NonEmptyArray StateElem    -- | holding state transition functions
  , currentMove :: MoveCount              -- | moves counter 
  , nextTurn :: Player                    -- | which Player would have next move
  }

-- | Message is the output from the Cell component 
data Message
  = IsClicked Position StateElem
  | IsReturned Int

--- Helpers ---

emptySquare :: Square
emptySquare = Nothing

emptyStateElem :: StateElem
emptyStateElem _ = Nothing

--- Square Component creation ---
sizeArray :: NonEmptyArray Int
sizeArray = 0 NEArray... 2

mkPosition :: Int -> Int -> Position
mkPosition r i
  | r `NEArray.elem` sizeArray
      && i `NEArray.elem` sizeArray = MkPosition { row: r, col: i }
  | otherwise = mkPosition frsRow frsCol
      where
      frsCol = NEArray.head sizeArray
      frsRow = NEArray.head sizeArray

