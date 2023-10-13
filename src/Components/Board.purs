module Components.Board
  ( mkBoardComponent
  ) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NEArray
import Data.BoardState (BoardState, Message(..))
import Data.BoardState as BoardState
import Data.Maybe (Maybe(..), maybe, isJust)
import Data.Player (Player, Square)
import Data.Position (Position)
import Data.Position as Position
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import Effect.Ref as Ref
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks (useTickEffect)
import Halogen.Hooks as Hooks
import Helpers (showBoardState)
import Type.Proxy (Proxy(..))
import Web.HTML.Common (ClassName(..))

--- Components Types 

data SquareQuery a = Tell (Maybe Player) a

--- Container Public API ---

-- | TODO: set game state a parameter and use them to get required data
mkSquareComponent
  :: forall m
   . MonadEffect m
  => Position
  -> H.Component SquareQuery Square Message m
mkSquareComponent pos = Hooks.component \rec initSq -> Hooks.do
  cellState /\ cellStateIdx <- Hooks.useState initSq

  Hooks.captures {} useTickEffect do
    log $ "Sqare was initied with: " <> show initSq
    pure Nothing

  Hooks.useQuery rec.queryToken case _ of
    Tell p a -> do -- | We expect update from parent via Tel      
      Hooks.put cellStateIdx p
      pure $ Just a

  let
    player = maybe "" show cellState
    isDisabled = isJust cellState

  Hooks.pure $
    HH.button
      [ HP.title player
      , HP.class_ $ ClassName "square"
      , HP.disabled isDisabled
      , HE.onClick \_ -> do
          when (not isDisabled) do
            Hooks.raise rec.outputToken $ IsClicked pos
      ]
      [ HH.text player ]

mkBoardComponent
  :: forall q m
   . MonadEffect m
  => Ref.Ref BoardState
  -> H.Component q BoardState Message m
mkBoardComponent boardRef = Hooks.component $ \rec initState -> Hooks.do
  brdState /\ brdIdx <- Hooks.useState initState
  Hooks.captures {} Hooks.useTickEffect do
    bSt <- liftEffect $ Ref.read boardRef
    Hooks.put brdIdx bSt
    log $ "Test me: " <> showBoardState bSt
    pure Nothing

  let
    mkSquareSlot stInit row colIdx = do
      pos <- Position.mkPosition row (colIdx :: Int)
      pure $ HH.slot _square pos (mkSquareComponent pos) (stInit pos) handleCell

    mkRow idx = do
      let stInit p = Array.head $ Array.mapMaybe (\f -> f p) brdState.history
      HH.div
        [ HP.class_ $ ClassName "boardRow"
        ]
        $ NEArray.mapMaybe (mkSquareSlot stInit idx) Position.positionsArray

    handleCell (IsClicked pos) = do
      st <- liftEffect $ Ref.read boardRef
      let
        player = st.nextTurn
        se = squareFn pos player
      newSt <- case BoardState.next pos se st of
        Just newState -> do
          liftEffect $ Ref.write newState boardRef
          -- | update state when we have a cell match with current Player 
          Hooks.tell rec.slotToken _square pos $ Tell $ Just player
          void $ log $ "We updated from handleCell to: " <> showBoardState newState
          pure newState
        Nothing -> do
          log $ "Bad move position detected: " <> show pos
          pure st

      Hooks.raise rec.outputToken $ HasWinner $ BoardState.hasBoardWinPositions newSt

    handleCell _ = do
      boardState <- liftEffect $ Ref.read boardRef
      void $ log $ "We could not capture proper message for cell change " <> show boardState.nextTurn
      pure unit

  Hooks.pure $ HH.div_ $ NEArray.toArray $ map mkRow Position.positionsArray
  where
  _square :: Proxy "square"
  _square = Proxy

  squareFn :: Position -> Player -> Position -> Square
  squareFn oldPos player sqPos
    | sqPos == oldPos = Just player
    | otherwise = Nothing
