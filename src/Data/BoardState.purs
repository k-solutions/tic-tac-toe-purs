module Data.BoardState 
  ( module BoardStateTypes 
  , hasBoardWinPositions
  , generateByPositions
  , init
  , next
  , reset
  ) where 

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEArray
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Foldable (foldM)

import Data.BoardStateTypes
import Data.BoardStateTypes as BoardStateTypes 
import Data.Player (Player, Square)
import Data.Player as Player
import Data.Position (Position, PositionsType(..))
import Data.Position as Position
import Helpers (hasWinPositions, isMoveValid)

--- BoardState API ---
generateByPositions :: NonEmptyArray Position 
                    -> NonEmptyArray Position 
                    -> Maybe BoardState
generateByPositions xArr oArr 
  = foldM mbBoardState init $ NEArray.zip xArr oArr
  where
   mbBoardState state (Tuple xPos oPos) = do
     let seFn player = const $ Just player 
     xState <- next xPos (seFn state.nextTurn)  state
     oState <- next oPos (seFn $ Player.next state.nextTurn) xState
     pure oState     

hasBoardWinPositions :: BoardState -> Square 
hasBoardWinPositions state 
  =  Array.head 
  $ NEArray.catMaybes 
  $  map (hasWinPositions Row state) Position.positionsArray 
  <> map (hasWinPositions Column state) Position.positionsArray
  <> NEArray.singleton (hasWinPositions Diagonal state 0)
  <> NEArray.singleton (hasWinPositions Diagonal state 1) 

init :: BoardState  
init = { history: [], nextTurn: Player.init }

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
