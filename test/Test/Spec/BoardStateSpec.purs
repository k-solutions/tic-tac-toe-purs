module Test.Spec.BoardStateSpec where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray(..))
import Data.BoardState as BoardState
import Data.Foldable (foldM)
import Data.Maybe (fromJust, Maybe(..))
import Data.Player (Player(..))
import Data.Position (Position(..), PositionsType(..))
import Data.Position as Position
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldNotEqual, shouldEqual)

-- import Partial.Unsafe (unsafePartial)

spec :: Spec Unit 
spec = describe "BoardState module tests" do
  it "showBoardState and extract st" do
     let gameState = BoardState.init 
         result = BoardState.showBoardState gameState
     result `shouldEqual` "Board state for moves: 0 and player: X"   

  it "On empty BoardState any generated move is proper" do
     let mbResult = Position.mkPosition 1 1     
         result = flip BoardState.isMoveValid BoardState.init <$> mbResult 
     result `shouldEqual` Just true 
 
  it "On non empty BoardState any generated move is proper" do
     let mbResult = Position.mkPosition 1 1     
         result = flip BoardState.isMoveValid BoardState.init <$> mbResult 
     result `shouldEqual` Just true 
 
  it "BoardState next generate proper state" do
     let elemFn = const $ Just X  
         mbResult = do
           pos <- Position.mkPosition 1 1     
           BoardState.next pos elemFn BoardState.init
         res = BoardState.showBoardState <$> mbResult 
     res `shouldEqual` Just "Board state for moves: 1 and player: O" 
         
  it "hasBoardWinPositions for Row with no winner should return Nothing" do
    let gameState = BoardState.init 
        result = BoardState.hasBoardWinPositions gameState 
    result `shouldEqual` Nothing 

  it "hasBoardWinPositions for Row with 3 moves X X X and a winner should not return Nothing" do
    let gameState = do
          posNEArr <- Position.generate Row 1  
          let elemFn = const $ Just X 
              scanFn state pos = BoardState.next pos elemFn state 
          foldM scanFn BoardState.init posNEArr     
        result = BoardState.hasBoardWinPositions <$> gameState
    result `shouldNotEqual` Nothing

  it "hasWinPositions for Row with 3 moves X X X and a winner should not return Nothing" do
    let gameState = do
          posNEArr <- Position.generate Row 1
          let elemFn = const $ Just X
              scanFn state pos = BoardState.next pos elemFn state
          foldM scanFn BoardState.init posNEArr
        result = BoardState.hasWinPositions Row <$> gameState <*> Just 1
    result `shouldNotEqual` Nothing
