module Helpers where

import Prelude

import Data.Array (mapMaybe)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEArray
import Data.BoardStateTypes (BoardState)
import Data.Maybe (Maybe(..), maybe, isJust)
import Data.Player (Player, Square)
import Data.Player as Player
import Data.Position (Position, PositionsType)
import Data.Position as Position
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))

--- Board State Helpers ---

showBoardState :: BoardState -> String
showBoardState { history, nextTurn, reset } = "Board state for moves: "
  <> show (Array.length history :: Int)
  <> ", for reset: "
  <> show (maybe 0 Array.length reset :: Int)
  <> " and player: "
  <> show nextTurn

fullBoardCheck
  :: Array (Position -> Maybe Player)
  -> Array Position
fullBoardCheck stateFns = toBoardPos gens stateFns
  where
  gens :: Array Position
  gens = Array.fold $ NEArray.mapMaybe (\p -> NEArray.toArray <$> Position.generate Position.Row p) Position.positionsArray

toBoardPos
  :: Array Position
  -> Array (Position -> Maybe Player)
  -> Array Position
toBoardPos pos stateFns = Array.mapMaybe go pos
  where
  go :: Position -> Maybe Position
  go p = Array.head $ Array.mapMaybe (\f -> const p <$> f p) stateFns

toBoard
  :: NonEmptyArray Position
  -> Array (Position -> Maybe Player) -- BoardState
  -> Array Player
toBoard pos stateFns = mapMaybe go $ NEArray.toArray pos
  where
  go :: Position -> Maybe Player
  go p = Array.head $ Array.mapMaybe (\f -> f p) stateFns

hasWinPositions
  :: PositionsType
  -> BoardState
  -> Int
  -> Square
hasWinPositions posType state i = do
  r <- Position.generate posType i
  let cRes = Array.foldr countEqual (Tuple 0 Nothing) $ toBoard r state.history

  case cRes of
    Tuple c (Just p) ->
      if c == NEArray.length r then Just p else Nothing
    _ -> Nothing

countEqual :: Player -> Tuple Int (Maybe Player) -> Tuple Int (Maybe Player)
countEqual p (Tuple c (Just p'))
  | p == p' = Tuple (c + 1) (Just p)
  | otherwise = Tuple 1 $ Just p
countEqual p (Tuple _ Nothing) = Tuple 1 $ Just p

isMoveValid 
  :: Position 
  -> Array (Position -> Maybe Player) 
  -> Boolean
isMoveValid pos state = Array.null board
  where
  board :: Array _
  board = Array.filter (\f -> isJust $ f pos) state
