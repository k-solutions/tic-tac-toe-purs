module Types  
  ( mkPosition
  , emptySquare
  , emptyStateElem
  , boardSize
  , sizeArray
  , rewindMoveCount
  , unMoveCount
  , initMoveCount
  , nextMoveCount
  , reverseMoveCount
  , nextPlayer
  , initPlayer
  , BoardState(..)
  , Player(..)
  , Square
  , Message(..)
  , StateElem
  , Position(..) 
  , MoveCount
  ) where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEArray
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Halogen.HTML (samp)
import Halogen.Hooks.Internal.Types (_memoValuesImpl)

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

derive instance Generic Position _

instance Show Position where
  show = genericShow

instance Eq Position where
  eq (MkPosition { row: r1, col: c1 }) (MkPosition { row: r2, col: c2 }) = r1 == r2 && c1 == c2 

instance Ord Position where
  compare (MkPosition {row: r1, col: c1}) (MkPosition {row: r2, col: c2}) 
    = case compare r1 r2 of
          EQ -> compare c1 c2
          LT -> LT
          GT -> GT

newtype MoveCount = MkMoveCount Int

instance Show MoveCount where
  show (MkMoveCount c) = "Counter: " <> show c   

-- | Board state to present to Game State
type BoardState =
  { history :: NonEmptyArray StateElem    -- | holding state transition functions
  , nextTurn :: Player                    -- | which Player would have next move
  }


-- | Message is the output from the Cell component 
data Message
  = IsClicked Position StateElem
  | HasWinner BoardState Square
  | IsReturned Int

-- derive instance Generic Message _

instance Show Message where
  show (IsClicked p _) = "Clicked on " <> show p 
  show (HasWinner _ s) = "Winner is " <> show s
  show (IsReturned i)  = "Revert state by " <> show i 

--- Helpers ---

rewindMoveCount :: Int -> MoveCount -> Maybe MoveCount
rewindMoveCount ix (MkMoveCount c)
  | ix < c = Just <<< MkMoveCount $ c - ix
  | otherwise = Nothing

unMoveCount :: MoveCount -> Int
unMoveCount (MkMoveCount i) = i

initMoveCount :: MoveCount
initMoveCount = MkMoveCount 0

nextMoveCount :: MoveCount -> MoveCount
nextMoveCount (MkMoveCount x) = MkMoveCount $ x + 1

reverseMoveCount :: MoveCount -> MoveCount
reverseMoveCount (MkMoveCount x) = MkMoveCount $ x - 1

nextPlayer :: Player -> Player
nextPlayer X = O
nextPlayer _ = X

initPlayer :: Player
initPlayer = X

emptySquare :: Square
emptySquare = Nothing

emptyStateElem :: StateElem
emptyStateElem _ = Nothing

--- Square Component creation ---

boardSize = 3

sizeArray :: NonEmptyArray Int
sizeArray = 0 NEArray... (boardSize - 1)

mkPosition :: Int -> Int -> Position
mkPosition r i
  | r `NEArray.elem` sizeArray
      && i `NEArray.elem` sizeArray = MkPosition { row: r, col: i }
  | otherwise = mkPosition frsRow frsCol
      where
      frsCol = NEArray.head sizeArray
      frsRow = NEArray.head sizeArray

