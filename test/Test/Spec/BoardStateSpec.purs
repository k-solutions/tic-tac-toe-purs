module Test.Spec.BoardStateSpec where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray(..))
import Data.BoardState as BoardState
import Data.Foldable (foldM)
import Data.Maybe ( Maybe(..))
import Data.Player (Player(..))
import Data.Position (PositionsType(..))
import Data.Position as Position
import Helpers (showBoardState)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldNotEqual, shouldEqual)

spec :: Spec Unit 
spec = describe "BoardState module tests" do
  it "hasBoardWinPositions for Row with 3 moves X X X and a winner should not return Nothing" do
    let gameState = do
          posNEArr <- Position.generate Row 1  
          let elemFn = const $ Just X 
              scanFn state pos = BoardState.next pos elemFn state 
          foldM scanFn BoardState.init posNEArr     
        result = BoardState.hasBoardWinPositions <$> gameState
    result `shouldNotEqual` Nothing

  it "BoardState next generate proper state" do
     let elemFn = const $ Just X  
         mbResult = do
           pos <- Position.mkPosition 1 1     
           BoardState.next pos elemFn BoardState.init
         res = showBoardState <$> mbResult 
     res `shouldEqual` Just "Board state for moves: 1 and player: O" 
 
