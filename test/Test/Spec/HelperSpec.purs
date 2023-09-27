module Test.Spec.HelpersSpec where

import Helpers
import Prelude

import Data.BoardState (BoardState)
import Data.BoardState as BoardState
import Data.Maybe (Maybe(..))

import Data.Position (PositionsType(..))
import Data.Position as Position
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldNotEqual, shouldEqual)

mbBoardState :: Maybe BoardState
mbBoardState = do
  posXNEArr <- Position.generate Row 1
  posONEArr <- Position.generate Row 3
  BoardState.generateByPositions posXNEArr posONEArr

spec :: Spec Unit 
spec = describe "Helpers module tests" do

  it "generateByPositions gives proper statee" do
     let result = showBoardState <$> mbBoardState  
     result `shouldNotEqual` Nothing   

  it "showBoardState makes state Showable" do
     let gameState = BoardState.init 
         result = showBoardState gameState
     result `shouldEqual` "Board state for moves: 0 and player: X"   

  it "On empty BoardState any generated move is valid" do
     let mbResult = Position.mkPosition 1 1     
         result = flip isMoveValid BoardState.init <$> mbResult 
     result `shouldEqual` Just true 
 
  it "On non empty BoardState any generated move is proper" do
     let result = do
           boardState <- mbBoardState
           pos <- Position.mkPosition 1 1     
           pure $ pos `isMoveValid` boardState 
     result `shouldEqual` Just true 
        
  it "hasWinPositions for Row with 3 moves X X X and a winner should not return Nothing" do
    let result = hasWinPositions Row <$> mbBoardState <*> Just 1
    result `shouldNotEqual` Nothing
