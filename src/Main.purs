module Main where

import Prelude

import Components.Game (mkGameComponent)
import Effect (Effect)
import Halogen.Aff as HAff
import Halogen.VDom.Driver (runUI)

--- Public API ---

main :: Effect Unit
main = HAff.runHalogenAff do
  body <- HAff.awaitBody
  void $ runUI mkGameComponent unit body
