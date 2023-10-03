module Test.Spec.HelpersSpec where

import Helpers
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
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldNotEqual, shouldEqual)

mbBoardState :: Maybe BoardState
mbBoardState = do
  posXNEArr <- Position.generate Row 1
  posONEArr <- sequence $ NEArray.singleton (Position.mkPosition 2 3)
    <> NEArray.singleton (Position.mkPosition 3 3)
    <> NEArray.singleton (Position.mkPosition 2 2)
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

  it "generateByPositions gives proper statee" do
    let result = showBoardState <$> mbBoardState
    result `shouldEqual` Just "Board state for moves: 6 and player: X"

  it "showBoardState makes state Showable" do
    let
      gameState = BoardState.init
      result = showBoardState gameState
    result `shouldEqual` "Board state for moves: 0 and player: X"

  it "On empty BoardState any generated move is valid" do
    let
      mbResult = Position.mkPosition 1 1
      result = flip isMoveValid BoardState.init <$> mbResult
    result `shouldEqual` Just true

  it "On non empty BoardState any generated move is proper" do
    let
      result0 = do
        pos1 <- Position.mkPosition 1 1
        stateX <- BoardState.next pos1 (squareFn pos1 Player.X) BoardState.init
        pure stateX
    (showBoardState <$> result0) `shouldEqual` Just "Board state for moves: 1 and player: O"
    let
      result = do
        pos2 <- Position.mkPosition 1 2
        pos3 <- Position.mkPosition 2 2
        stateX <- result0
        stateO <- BoardState.next pos2 (squareFn pos2 Player.O) stateX
        pure $ pos3 `isMoveValid` stateO
    result `shouldEqual` Just true

  it "In winning BoardState any position over exisitng position should be invalid" do
    let
      mbResult = do
        pos <- Position.mkPosition 1 3
        state <- mbBoardState
        pure $ pos `isMoveValid` state
    mbResult `shouldEqual` Just false

  it "Winning BoardState any generated move is proper" do
    let
      result = do
        boardState <- mbBoardState
        pos <- Position.mkPosition 2 1
        pure $ pos `isMoveValid` boardState
    result `shouldEqual` Just true

  it "toBoard for Row with 3 moves X X X should return proper players" do
    let
      pRow1 = Position.generate Row 1
      result = toBoard <$> pRow1 <*> mbBoardState
    result `shouldEqual` Just (Array.replicate 3 $ Player.X :: Array Player.Player)

  it "hasWinPositions for Row with 3 moves X X X and a winner should not return Nothing" do
    let result = join $ hasWinPositions Row <$> mbBoardState <*> Just 1
    result `shouldEqual` Just Player.X
