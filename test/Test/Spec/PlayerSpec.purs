module Test.Spec.PlayerSpec where

import Prelude
import Data.Player
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "Player module tests" do
  it "init should return X" do
    let result = init
    result `shouldEqual` X

  it "next X should return O" do
    let result = next X
    result `shouldEqual` O

  it "next O should return X" do
    let result = next O
    result `shouldEqual` X

  it "rewind 0 X should return X" do
    let result = rewind 0 X
    result `shouldEqual` X


  it "rewind 3 X should return O" do
    let result = rewind 3 X
    result `shouldEqual` O 

  it "rewind 2 X should return X" do
    let result = rewind 2 X
    result `shouldEqual` X
