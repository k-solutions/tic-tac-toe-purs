module Data.BoardState 
  ( Message(..)
  , BoardState 
  , StateElem

  , hasBoardWinPositions
  , hasWinPositions 
  , countEqual
  , isMoveValid

  , init
  , next
  , reset
  , showBoardState  
  ) where 

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray(..))

import Data.Array.NonEmpty as NEArray
import Data.Maybe (Maybe(..), isJust)
import Data.Tuple (Tuple(..))
import Data.Player (Player, Square)
import Data.Player as Player
import Data.Position (Position, PositionsType(..))
import Data.Position as Position
import Data.Traversable (traverse)

--- BoardState Types ---

type BoardState =   
  { history   :: Array (Position -> Maybe Player)    -- | holding state transition functions
  , nextTurn  :: Player                              -- | which Player would have next move
  }

-- instance Show BoardState where
showBoardState :: BoardState -> String
showBoardState {history, nextTurn} = "Board state for moves: " <> show (Array.length history :: Int)  <> " and player: " <> show nextTurn 


-- | Message is the output from the Cell component
data Message
  = IsClicked Position -- StateElem
  | HasWinner BoardState Square
  | IsReturned Int

instance Show Message where
  show (IsClicked p )  = "Clicked on " <> show p
  show (HasWinner _ s) = "Winner is " <> show s
  show (IsReturned i)  = "Revert state by " <> show i

type StateElem = Position -> Square

--- BoardState API ---

hasBoardWinPositions :: BoardState -> Square 
hasBoardWinPositions state 
  =  Array.head 
  $ NEArray.catMaybes 
  $  map (hasWinPositions Row state) Position.positionsArray 
  <> map (hasWinPositions Column state) Position.positionsArray
  <> NEArray.singleton (hasWinPositions Diagonal state 0)
  <> NEArray.singleton (hasWinPositions Diagonal state 1) 

hasWinPositions :: PositionsType
                -> BoardState
                -> Int
                -> Square  
hasWinPositions posType state i = do
      r <- Position.generate posType i
      pArr <- traverse go r
      let cRes = Array.foldr countEqual (Tuple 0 Nothing) $ NEArray.toArray pArr
     
      case cRes of
        Tuple c  (Just p) -> 
          if c == NEArray.length r then Just p else Nothing 
        _ -> Nothing  
  where        
    go :: Position -> Maybe Player  
    go p = Array.head $ Array.mapMaybe (\f -> f p) state.history

countEqual :: Player -> Tuple Int (Maybe Player) -> Tuple Int (Maybe Player)
countEqual p (Tuple c (Just p')) 
  | p == p' = Tuple (c + 1) (Just p)
  | otherwise = Tuple 1 $ Just p 
countEqual p (Tuple _ Nothing) = Tuple 1 $ Just p    
-- countEqual _ _ = Tuple 0 Nothing 

-- instance AsBoardState Position Player where
init :: BoardState  
init = { history: [], nextTurn: Player.init }

isMoveValid :: Position -> BoardState -> Boolean
isMoveValid pos state = Array.null state.history || hasOtherMove   
  where
      hasOtherMove = Array.elem (Player.next state.nextTurn) board 

      board :: Array Player 
      board = Array.mapMaybe (\f -> f pos) state.history 

next :: Position 
     -> (Position -> Maybe Player) 
     -> BoardState  
     -> Maybe BoardState 
next pos move s  
    | pos `isMoveValid` s = Just $ s { history = move Array.: s.history, nextTurn = Player.next s.nextTurn }
    | otherwise = Nothing

reset :: Int -> BoardState -> Maybe BoardState 
reset idx state = do
     let r = Player.rewind idx state.nextTurn 
     pure $ state { history = Array.drop idx state.history, nextTurn = r }
