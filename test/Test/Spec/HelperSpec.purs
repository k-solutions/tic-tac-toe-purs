module Test.Spec.HelperSpec where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEArray
import Data.BoardState (BoardState)
import Data.BoardState as BoardState
import Data.Maybe (Maybe(..))
import Data.Player as Player
import Data.Position (PositionsType(..))
import Data.Position as Position
import Data.Traversable (sequence)
import Helpers as Helpers
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldNotEqual, shouldEqual)

mbBoardState :: PositionsType -> Maybe BoardState
mbBoardState pt = do
  posXNEArr <- Position.generate pt 1
  posONEArr <- sequence $ NEArray.singleton (Position.mkPosition 2 3)
    <> NEArray.singleton (Position.mkPosition 3 3)
    <> NEArray.singleton (Position.mkPosition 3 2)
  BoardState.generateByPositions posXNEArr posONEArr

squareFn
  :: Position.Position
  -> Player.Player
  -> Position.Position
  -> Maybe Player.Player
squareFn oldPos player sqPos
  | sqPos == oldPos = Just player
  | otherwise = Nothing

spec :: Spec Unit
spec = describe "Helpers module tests" do

  it "On Full state reset all reseted positions are valid" do
    let
      result = do
        posArr <- Position.generate Position.Row 1
        state <- mbBoardState Row
        newState <- BoardState.reset 6 state 
        pure $ NEArray.all (\ pos -> pos `Helpers.isMoveValid` newState.history) posArr
    result `shouldEqual` Just true 


  it "generateByPositions gives proper statee" do
    let result = Helpers.showBoardState <$> mbBoardState Row
    result `shouldEqual` Just "Board state for moves: 6, for reset: 0 and player: X"

  it "showBoardState makes state Showable" do
    let
      gameState = BoardState.init
      result = Helpers.showBoardState gameState
    result `shouldEqual` "Board state for moves: 0, for reset: 0 and player: X"

  it "On empty BoardState any generated move is valid" do
    let
      mbResult = Position.mkPosition 1 1
      state = BoardState.init 
      result = flip Helpers.isMoveValid state.history <$> mbResult
    result `shouldEqual` Just true

  it "On non empty BoardState any generated move is proper" do
    let
      result0 = do
        pos1 <- Position.mkPosition 1 1
        stateX <- BoardState.next pos1 (squareFn pos1 Player.X) BoardState.init
        pure stateX
    (Helpers.showBoardState <$> result0) `shouldEqual` Just "Board state for moves: 1, for reset: 0 and player: O"
    let
      result = do
        pos2 <- Position.mkPosition 1 2
        pos3 <- Position.mkPosition 2 2
        stateX <- result0
        stateO <- BoardState.next pos2 (squareFn pos2 Player.O) stateX
        pure $ pos3 `Helpers.isMoveValid` stateO.history
    result `shouldEqual` Just true

  it "On winning BoardState any position over exisitng position should be invalid" do
    let
      mbResult = do
        pos <- Position.mkPosition 1 3
        state <- mbBoardState Row
        pure $ pos `Helpers.isMoveValid` state.history
    mbResult `shouldEqual` Just false

  it "On Winning BoardState any generated move is proper" do
    let
      result = do
        boardState <- mbBoardState Row
        pos <- Position.mkPosition 2 1
        pure $ pos `Helpers.isMoveValid` boardState.history
    result `shouldEqual` Just true

  it "On state reset our reseted position is valid" do
    let
      result = do
        pos   <- Position.mkPosition 3 2  
        state <- mbBoardState Row
        newState <- BoardState.reset 1 state 
        pure $ pos `Helpers.isMoveValid`  newState.history
    result `shouldEqual` Just true 

  it "toBoard for Row with 3 moves X X X should return proper players" do
    let
      result = do
        pRow1 <- Position.generate Row 1
        state <- mbBoardState Row
        pure $ Helpers.toBoard pRow1 state.history
    result `shouldEqual` Just (Array.replicate 3 Player.X :: Array Player.Player)

  it "hasWinPositions for Row with 3 moves X X X and a winner should return X" do
    let result = join $ Helpers.hasWinPositions Row <$> mbBoardState Row <*> Just 1
    result `shouldEqual` Just Player.X

  it "hasWinPositions for Diagonal with 3 moves X X X and a winner should not return Nothing" do
    let result = join $ Helpers.hasWinPositions Diagonal <$> mbBoardState Diagonal <*> Just 1
    result `shouldEqual` Just Player.X

  it "hasWinPositions for Column with 3 moves X X X and a winner should not return Nothing" do
    let result = join $ Helpers.hasWinPositions Column <$> mbBoardState Column <*> Just 1
    result `shouldEqual` Just Player.X
