module Data.BoardState
  ( module BoardStateTypes
  , hasBoardWinPositions
  , generateByPositions
  , init
  , next
  , reset
  ) where

import Data.BoardStateTypes
import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEArray
import Data.Array.NonEmpty.Internal (NonEmptyArray(..))
import Data.BoardStateTypes as BoardStateTypes
import Data.Foldable (foldM)
import Data.Maybe (Maybe(..))
import Data.Player (Player, Square)
import Data.Player as Player
import Data.Position (Position, PositionsType(..))
import Data.Position as Position
import Data.Tuple (Tuple(..))
import Helpers (hasWinPositions, isMoveValid)

--- BoardState API ---
generateByPositions
  :: NonEmptyArray Position
  -> NonEmptyArray Position
  -> Maybe BoardState
generateByPositions xArr oArr = foldM mbBoardState init resArr
  where
  lenX = NEArray.length xArr

  lenO = NEArray.length oArr

  seFn pos player newPos
    | pos == newPos = Just player
    | otherwise = Nothing

  resArr
    | lenX > lenO = NEArray.zip (map Just xArr) $ map Just oArr <> NEArray.replicate (lenX - lenO) Nothing
    | lenX < lenO = NEArray.zip (map Just xArr <> NEArray.replicate (lenO - lenX) Nothing) $ map Just oArr
    | otherwise = NEArray.zip (map Just xArr) $ map Just oArr

  mbBoardState state (Tuple (Just xPos) (Just oPos)) = do
    xState <- next xPos (seFn xPos state.nextTurn) state
    oState <- next oPos (seFn oPos $ Player.next state.nextTurn) xState
    pure oState
  mbBoardState state (Tuple Nothing (Just oPos)) = do
    oState <- next oPos (seFn oPos $ Player.next state.nextTurn) state
    pure oState
  mbBoardState state (Tuple (Just xPos) Nothing) = do
    newState <- next xPos (seFn xPos $ Player.next state.nextTurn) state
    pure newState
  mbBoardState state (Tuple Nothing Nothing) = pure state

hasBoardWinPositions :: BoardState -> Square
hasBoardWinPositions state = Array.head
  $ NEArray.catMaybes
  $ map (hasWinPositions Row state) Position.positionsArray
      <> map (hasWinPositions Column state) Position.positionsArray
      <> NEArray.singleton (hasWinPositions Diagonal state 0)
      <> NEArray.singleton (hasWinPositions Diagonal state 1)

init :: BoardState
init = { history: [], nextTurn: Player.init }

next
  :: Position
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
