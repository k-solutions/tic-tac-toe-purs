module Components.Board
  ( mkBoardComponent
  , BoardQuery(..) 
  ) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEArray
import Data.BoardState (BoardState, Message(..), StateElem)
import Data.BoardState as BoardState
import Data.Maybe (Maybe(..), maybe, isJust)
import Data.Player (Player, Square)
import Data.Player as Player
import Data.Position (Position)
import Data.Position as Position
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML (i, samp)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Type.Proxy (Proxy(..))
import Web.HTML.Common (ClassName(..))

--- Components Types 

data SquareQuery a = Tell Player a  --    

--- Container Public API ---

-- | TODO: set game state a parameter and use them to get required data
mkSquareComponent
  :: forall i m
   . MonadEffect m
  => Position
  -> H.Component SquareQuery i Message m
mkSquareComponent pos = Hooks.component \rec _ -> Hooks.do
  cellState /\ cellStateIdx <- Hooks.useState (Nothing :: Square)   -- | state is Maybe Player

  Hooks.useQuery rec.queryToken case _ of
    Tell p a -> do                                 -- | We expect update from parent via Tell
      Hooks.put cellStateIdx $ Just p   
      pure $ Just a      

  let player = maybe "" show cellState -- pos
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

data BoardQuery a = GetState (BoardState -> a)

mkBoardComponent
  :: forall i m
   . MonadEffect m
  => H.Component BoardQuery i Message m
mkBoardComponent = Hooks.component $ \rec _ -> Hooks.do
  boardState /\ boardStateIdx <- Hooks.useState BoardState.init

  Hooks.useQuery rec.queryToken case _ of
    GetState queryFn -> do
      bState <- Hooks.get boardStateIdx
      pure $ Just $ queryFn bState

  let
        mkSquareSlot row colIdx = do
           pos <- Position.mkPosition row (colIdx :: Int) 
           pure $ HH.slot _square pos (mkSquareComponent pos) unit handleCell 

        mkRow idx = do
          HH.div
            [ HP.class_ $ ClassName "boardRow"
            ]
            $ NEArray.mapMaybe (mkSquareSlot idx) Position.positionsArray -- sizedArr

        handleCell (IsClicked pos) = do
            st <- Hooks.get boardStateIdx 
            let player = st.nextTurn 
                se = squareFn pos player  
            case BoardState.next pos se st of    
                Just newState -> do 
                  Hooks.put boardStateIdx newState 
                  -- | update state when we have a cell match with next turn
                  Hooks.tell rec.slotToken _square pos $ Tell player                                                           
                  void <<< log $ "We updated Board State with move from "  <> show player
                Nothing -> 
                  void $ log $ "Bad move position detected: " <> show pos 

            Hooks.raise rec.outputToken $ HasWinner boardState $ BoardState.hasBoardWinPositions boardState

        handleCell _  = do
          void $ log $ "We could not capture proper message for cell change " <> show boardState.nextTurn 
          pure unit

  Hooks.pure $ HH.div_ $ NEArray.toArray $ map mkRow Position.positionsArray  --  sizedArr
  where
  _square :: Proxy "square"
  _square = Proxy

  squareFn :: Position -> Player -> Position -> Square
  squareFn oldPos player sqPos 
    | sqPos == oldPos = Just player
    | otherwise = Nothing   
