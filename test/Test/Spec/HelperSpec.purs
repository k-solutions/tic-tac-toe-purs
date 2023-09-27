module Test.Spec.HelpersSpec where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray(..))
import Data.BoardState as BoardState
import Data.Foldable (foldM)
import Data.Maybe (fromJust, Maybe(..))
import Data.Player (Player(..))
import Data.Position (PositionsType(..))
import Data.Position as Position
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldNotEqual, shouldEqual)
import Helpers 

spec :: Spec Unit 
spec = describe "Helpers module tests" do
  it "showBoardState makes state Showable" do
     let gameState = BoardState.init 
         result = showBoardState gameState
     result `shouldEqual` "Board state for moves: 0 and player: X"   

  it "On empty BoardState any generated move is valid" do
     let mbResult = Position.mkPosition 1 1     
         result = flip isMoveValid BoardState.init <$> mbResult 
     result `shouldEqual` Just true 
 
  it "On non empty BoardState any generated move is proper" do
     let mbResult = Position.mkPosition 1 1     
         result = flip isMoveValid BoardState.init <$> mbResult 
     result `shouldEqual` Just true 
        
  it "hasWinPositions for Row with 3 moves X X X and a winner should not return Nothing" do
    let gameState = do
          posNEArr <- Position.generate Row 1
          let elemFn = const $ Just X
              scanFn state pos = BoardState.next pos elemFn state
          foldM scanFn BoardState.init posNEArr
        result = hasWinPositions Row <$> gameState <*> Just 1
    result `shouldNotEqual` Nothing
