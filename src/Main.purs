module Main where

import Prelude

import Components.Game (mkGameComponent)
-- import Data.Tuple (Tuple(..))
import Effect (Effect)
-- import Effect.Class (class MonadEffect)
-- import Halogen as H
import Halogen.Aff as HAff
import Halogen.VDom.Driver (runUI)

--- Helpers ---

--- Public API ---

main :: Effect Unit
main = HAff.runHalogenAff do
  body <- HAff.awaitBody
  void $ runUI mkGameComponent unit body
