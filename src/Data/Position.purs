module Data.Position 
  ( Position
  , PositionsType(..)
  , mkPosition
  , mkPositions
  , positionsArray 
  , generate
  ) where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray(..))
import Data.Array.NonEmpty as NEArray
import Data.Array.NonEmpty.Internal (NonEmptyArray(..))
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)

import Data.Player (Player(..))

--- Data Types ---

data PositionsType = Row
                   | Column
                   | Diagonal

newtype Position = MkPosition
  { row :: Int --| RowIdx
  , col :: Int --| ColIdx
  }

-- derive instance Generic Position _

instance Show Position where
  show (MkPosition p) = "(" <> show p.row <> ", " <> show p.col <> ")"  -- genericShow

instance Eq Position where
  eq (MkPosition { row: r1, col: c1 }) (MkPosition { row: r2, col: c2 }) = r1 == r2 && c1 == c2

instance Ord Position where
  compare (MkPosition {row: r1, col: c1}) (MkPosition {row: r2, col: c2})
    = case compare r1 r2 of
          EQ -> compare c1 c2
          LT -> LT
          GT -> GT

--- Positon API --- 
positionsArray :: NonEmptyArray Int
positionsArray = 1 NEArray...3

sizedMono :: Int -> NonEmptyArray Int 
sizedMono i = NEArray.replicate 3 i  

mkPosition :: Int -> Int -> Maybe Position
mkPosition row col 
    | row `NEArray.elem` positionsArray && col `NEArray.elem` positionsArray = Just $ MkPosition {row: row, col: col}
    | otherwise = Nothing 

mkPositions :: NonEmptyArray Int -> NonEmptyArray Int -> Maybe (NonEmptyArray Position)
mkPositions rows cols = sequence $ NEArray.zipWith mkPosition rows cols 

generate :: PositionsType -> Int -> Maybe (NonEmptyArray Position)
generate Row i = mkPositions (sizedMono i)  positionsArray       
generate Column i  = mkPositions positionsArray $ sizedMono i  
generate Diagonal i 
    | i == 0 = mkPositions positionsArray (positionsArray :: NonEmptyArray Int)
    | otherwise = mkPositions (positionsArray :: NonEmptyArray Int) $ NEArray.reverse positionsArray    
