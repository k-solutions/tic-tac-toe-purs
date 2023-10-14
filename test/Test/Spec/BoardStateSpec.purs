module Test.Spec.BoardStateSpec where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NEArray
import Data.BoardState (BoardState)
import Data.BoardState as BoardState
import Data.Maybe (maybe, Maybe(..))
import Data.Player (Player(..))
import Data.Player as Player
import Data.Position (PositionsType(..))
import Data.Position as Position
import Data.Traversable (sequence)
import Helpers (showBoardState)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

mbBoardState :: Maybe BoardState
mbBoardState = do
  posXNEArr <- Position.generate Row 1
  posONEArr <- Position.generate Row 3
  BoardState.generateByPositions posXNEArr posONEArr

mbShowState = maybe "" showBoardState

spec :: Spec Unit
spec = describe "BoardState module tests" do

  it "Board State reset to inital should be at inital state" do
    let
      result = do
        state <- mbBoardState
        BoardState.reset 6 state
    mbShowState result `shouldEqual` "Board state for moves: 0, for reset: 6 and player: X"

  it "Board State reset with bigger index than current state should be Nothing" do
    let
      result = do
        state <- mbBoardState
        BoardState.reset 10 state
    mbShowState result `shouldEqual` ""

  it "Board State reset 1 should give one step back state" do
    let
      result = do
        state <- mbBoardState
        BoardState.reset 1 state
    mbShowState result `shouldEqual` "Board state for moves: 5, for reset: 1 and player: O"

  it "Board State reset 0 should give same state" do
    let
      result = do
        state <- mbBoardState
        BoardState.reset 0 state
    mbShowState result `shouldEqual` "Board state for moves: 6, for reset: 0 and player: X"

  it "hasBoardWinPositions for Row with 3 moves X X X and a winner should not return Nothing" do
    let result = join $ BoardState.hasBoardWinPositions <$> mbBoardState
    result `shouldEqual` Just Player.X

  it "hasBoardWinPositions for Diagonal 1 with moves X X X has a winner X" do
    let
      mbDiagState = do
        posXNA <- Position.generate Diagonal 1
        posONA <- join $ NEArray.fromArray <$> sequence [ Position.mkPosition 3 2, Position.mkPosition 3 3, Position.mkPosition 2 3 ]
        BoardState.generateByPositions posXNA posONA
      result = join $ BoardState.hasBoardWinPositions <$> mbDiagState
    result `shouldEqual` Just Player.X

  it "BoardState next generate proper state" do
    let
      elemFn = const $ Just X
      mbResult = do
        pos <- Position.mkPosition 1 1
        BoardState.next pos elemFn BoardState.init
      res = showBoardState <$> mbResult
    res `shouldEqual` Just "Board state for moves: 1, for reset: 0 and player: O"
