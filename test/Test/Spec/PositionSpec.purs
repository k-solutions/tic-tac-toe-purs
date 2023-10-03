module Test.Spec.PositionSpec where

import Data.Position
import Prelude

import Data.Array.NonEmpty as NEArray
import Data.Maybe (Maybe(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Web.HTML.HTMLProgressElement (position)

spec :: Spec Unit
spec = describe "Position module tests" do
  it "mkPosition with invalid inputs should return Nothing" do
    let result = mkPosition 4 5
    result `shouldEqual` Nothing

  it "generate Row 2 should return Just [MkPosition 2 1, MkPosition 2 2, MkPosition 2 3]" do
    let result = generate Row 2
    result `shouldEqual` mkPositions (NEArray.replicate 3 2) positionsArray

  it "generate Column 3 should return Just [MkPosition 1 3, MkPosition 2 3, MkPosition 3 3]" do
    let result = generate Column 3
    result `shouldEqual` (mkPositions positionsArray $ NEArray.replicate 3 3)

  it "generate Diagonal 0 should return Just [MkPosition 1 1, MkPosition 2 2, MkPosition 3 3]" do
    let result = generate Diagonal 0
    result `shouldEqual` mkPositions positionsArray positionsArray

  it "generate Diagonal 1 should return Just [MkPosition 1 3, MkPosition 2 2, MkPosition 3 1]" do
    let result = generate Diagonal 1
    result `shouldEqual` (mkPositions positionsArray $ NEArray.reverse positionsArray)
