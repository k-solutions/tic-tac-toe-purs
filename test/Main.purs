module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Aff (launchAff_)

import Test.Spec.BoardStateSpec as BoardStateSpec
import Test.Spec.PositionSpec as PositionSpec
import Test.Spec.PlayerSpec as PlayerSpec
import Test.Spec.HelperSpec as HeleperSpec
import Test.Spec.Runner (runSpec)
import Test.Spec.Reporter.Console (consoleReporter)

main :: Effect Unit
main = launchAff_ do
  runSpec [ consoleReporter ] specs

  where
  specs = do
    BoardStateSpec.spec
    PositionSpec.spec
    PlayerSpec.spec
    HeleperSpec.spec
