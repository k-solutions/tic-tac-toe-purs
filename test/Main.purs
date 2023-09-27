module Test.Main where

import Data.Maybe
import Prelude
import Data.Array.NonEmpty.Internal (NonEmptyArray(..))
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Aff (launchAff_)

import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)
import Test.Spec (describe, it)
import Test.Spec.Discovery (discover)
import Test.Spec.Runner (runSpec)
import Test.Spec.Reporter.Console (consoleReporter)

import Test.Spec.BoardStateSpec as BoardStateSpec 
import Test.Spec.PositionSpec as PositionSpec
import Test.Spec.PlayerSpec as PlayerSpec

main :: Effect Unit
main = launchAff_ do
  runSpec [consoleReporter] specs

  where
    specs = do
      BoardStateSpec.spec
      PositionSpec.spec
      PlayerSpec.spec
