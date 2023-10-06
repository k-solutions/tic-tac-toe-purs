module Components.Game
  ( mkGameComponent
  ) where

import Prelude

import Components.Board (BoardQuery(..))
import Components.Board as Board
import Data.Array as Array
import Data.BoardState (Message(..))
import Data.BoardState as BoardState
import Data.Counter (Counter)
import Data.Counter as Counter
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Player (Square, Player)
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Type.Proxy (Proxy(..))
import Web.HTML.Common (ClassName(..))
import Helpers as Helpers

--- Game Component Types ---

type GameState =
  { currentMove :: Int        -- Counter  -- | moves counter 
  , winner :: Square          -- | which Player is winner
  }

--- Public API ---

mkHistoryButton
  :: forall q i m
   . MonadEffect m
  => String
  -> Int
  -> H.Component q i Message m
mkHistoryButton txt ix = Hooks.component $ \rec _ -> Hooks.do
  Hooks.pure $
    HH.button
      [ HP.title txt
      , HE.onClick \_ ->
          Hooks.raise rec.outputToken $ IsReturned ix
      ]
      [ HH.text txt
      ]

mkGameComponent
  :: forall q i o m
   . MonadEffect m
  => H.Component q i o m
mkGameComponent = Hooks.component $ \rec _ -> Hooks.do
  gameState /\ gameStateIdx <- Hooks.useState (initState :: GameState)
  boardState /\ boardStateIdx <- Hooks.useState (BoardState.init)

  Hooks.captures { } Hooks.useTickEffect do
    mbState <- Hooks.query rec.slotToken _board unit $ H.mkRequest GetState
    case mbState of
      Just bState -> do
        Hooks.put boardStateIdx bState
        log $ "Updated from GameComponent capure with: " <> Helpers.showBoardState bState
        pure Nothing 
      Nothing -> pure Nothing

  let
    moves = Array.fromFoldable $ Array.mapWithIndex (\ix _ -> setSlot ix) boardState.history
      where
      setSlot ix = HH.slot _history ix (mkHistoryButton ("Move # " <> show ix) ix) unit handleHistory

    rewindState :: Int -> GameState -> Maybe GameState
    rewindState ix st 
      | st.currentMove >= ix = do
          let rMove = st.currentMove - ix   -- rMove <- Counter.rewind ix st.currentMove
          pure $ st { currentMove = rMove }
      | otherwise = Nothing   

    handleHistory (IsReturned ix) = do
      oldSt <- Hooks.get gameStateIdx
      case rewindState ix oldSt of
        Just st -> do
          Hooks.modify_ boardStateIdx $ (\s -> fromMaybe s $ BoardState.reset ix s)   
          Hooks.put gameStateIdx st
        Nothing -> do
          void $ log $ "Could not rewind state with " <> show gameState.currentMove
          pure unit
    handleHistory m = do
      void <<< log $ "wrong message captured" <> show m
      pure unit

    handleBoard (HasWinner w) = do
      Hooks.modify_ gameStateIdx $ \st ->
        st { winner = w, currentMove = Array.length boardState.history }
    handleBoard _ = do
      log "Board state change" 
      pure unit

    status :: String
    status =
      case gameState.winner of
        Just winner -> "Winner is : " <> show winner
        Nothing -> "Next move is: " <> show (boardState.nextTurn :: Player) -- | We need to get previous state                                                                    
  Hooks.pure do
    HH.div
      [ HP.class_ $ ClassName "game"
      ]
      [ HH.slot _board unit Board.mkBoardComponent unit handleBoard
      , HH.div
          [ HP.class_ $ ClassName "gameInfo"
          ]
          [ HH.div_ [ HH.text status ]
          , HH.div_ moves
          ]
      ]
  where
  _board :: Proxy "board"
  _board = Proxy

  _history :: Proxy "history"
  _history = Proxy

  initState :: GameState
  initState =
    { currentMove: 0  -- Counter.init
    , winner: Nothing
    }
