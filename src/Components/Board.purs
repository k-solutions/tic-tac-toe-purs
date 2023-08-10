module Components.Board
  ( mkBoardComponent
  , nextPlayer
  ) where

import Prelude

import Components.Square (MoveCount(..), GameState(..), Message(..), Player(..), StateElem, Position, emptyStateElem, mkPosition, mkSquareComponent, sizeArray)
import Data.Array ((..))
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEArray
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect)
import Effect.Console (log)
import Halogen as H
import Halogen.Aff as HAff
import Halogen.HTML (element)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Type.Proxy (Proxy(..))
import Web.HTML.Common (ClassName(..))

--- Data  Types ---

--- Helpers ---
_square :: Proxy "square"
_square = Proxy

nextPlayer :: Player -> Player
nextPlayer X = O
nextPlayer _ = X

updateState
  :: StateElem
  -> GameState
  -> GameState
updateState se st =
  { history: se NEArray.: st.history
  , currentMove: nextMove st.currentMove
  , nextTurn: nextPlayer st.nextTurn
  }
  where
  nextMove (MkMoveCount x) = MkMoveCount $ x + 1

--mkCreateBoardPositions :: Array (Array Position)
--mkCreateBoardPositions 
--  = map (\r -> map (mkPosition r) sizeArray) rows
--  where
--    rows = map MkRow sizeArray  
--

--- Container Public API ---

mkBoardComponent
  :: forall q i o m
   . MonadEffect m
  => Tuple GameState _
  -> H.Component q i o m
mkBoardComponent gameSt@(gameState /\ gameStateIdx) = Hooks.component $ \_ _ -> Hooks.do
  let
    handleCellWithPosition row = handleCell <<< mkPosition row
    mkSquareSlot row colIdx = HH.slot _square (mkPosition row colIdx) (mkSquareComponent (mkPosition row colIdx) gameSt) unit $ handleCellWithPosition row colIdx
    mkRow idx = do
      HH.div
        [ HP.class_ $ ClassName "boardRow"
        ]
        $ NEArray.toArray
        $ map (mkSquareSlot idx) sizeArray

    handleCell pos (IsClicked move se) = do
      when (pos == move) do
        Hooks.modify_ gameStateIdx $ updateState se -- | update state when we have a cell match with next turn 
    handleCell _ _ = pure unit

  Hooks.pure $ HH.div_ $ NEArray.toArray $ map mkRow sizeArray
--  where
