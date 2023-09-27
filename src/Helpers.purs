module Helpers  where 

import Prelude

import Data.Array as Array
-- import Data.Array.NonEmpty (NonEmptyArray(..))
import Data.Array.NonEmpty as NEArray
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Traversable (traverse)

import Data.Player (Player, Square)
import Data.Player as Player
import Data.Position (Position, PositionsType)
import Data.Position as Position
import Data.BoardStateTypes (BoardState)

--- Board State Helpers ---

showBoardState :: BoardState -> String
showBoardState {history, nextTurn} = "Board state for moves: " <> show (Array.length history :: Int)  <> " and player: " <> show nextTurn

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

isMoveValid :: Position -> BoardState -> Boolean
isMoveValid pos state = Array.null state.history || hasOtherMove   
  where
      hasOtherMove = Array.elem (Player.next state.nextTurn) board 
      board :: Array Player 
      board = Array.mapMaybe (\f -> f pos) state.history 
