module Components.Board
  ( mkBoardComponent
  ) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEArray
import Data.Array.NonEmpty.Internal (NonEmptyArray(..))
import Data.Maybe (Maybe(..), maybe)
import Data.NonEmpty (NonEmpty(..))
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
import Types (BoardState(..), Message(..), Player(..), Position, StateElem, Square, boardSize, emptyStateElem, mkPosition, nextMoveCount, nextPlayer, sizeArray)
import Web.HTML.Common (ClassName(..))

--- Container Public API ---

-- | TODO: set game state a parameter and use them to get required data
mkSquareComponent
  :: forall query i m
   . MonadEffect m
  => Position
  -> BoardState 
  -> H.Component query i Message m
mkSquareComponent pos parentState = Hooks.component \rec _ -> Hooks.do
  cellState /\ cellStateIdx <- Hooks.useState emptyStateElem
  let player = maybe "" show $ cellState pos
  Hooks.pure $
    HH.button
      [ HP.title player
      , HP.class_ $ ClassName "square"
      , HP.disabled $ isDisabled player
      , HE.onClick \_ -> do
          when (not isDisabled player) do
            newSt <- Hooks.modify cellStateIdx <<< const $ squareFn parentState.nextTurn -- | nextPlayer 
            Hooks.raise rec.outputToken $ IsClicked pos newSt
      ]
      [ HH.text player ]
  where
  squareFn :: Player -> Position -> Square
  squareFn player sqPos 
    | sqPos == pos = Just player
    | otherwise = Nothing   

  isDisabled p
    | p == "" = false
    | otherwise = true

mkBoardComponent
  :: forall q i m
   . MonadEffect m
  => BoardState
  -> H.Component q i Message m
mkBoardComponent gameState = Hooks.component $ \rec _ -> Hooks.do
  boardState /\ boardStateIdx <- Hooks.useState gameState
  let
    allAvailableMoves :: Array Player
    allAvailableMoves = Array.concatMap go 
                      $ NEArray.toArray mkWinningBoardPositions
      where
        getNew i (Tuple c (Just nextI)) 
          | i == nextI = Tuple (c + 1) $ Just i
          | otherwise =  Tuple c Nothing 
        getNew i (Tuple _ Nothing) = Tuple 1 $ Just i

        getWinnigMoves :: Array Player -> Square
        getWinnigMoves = extractIfFull <<< Array.foldr getNew (Tuple 0 Nothing)
          where
           extractIfFull (Tuple c s) 
            | c == boardSize = s
            | otherwise = Nothing
       
        go :: NonEmptyArray Position -> Array Player
        go wps = NEArray.mapMaybe (\fn -> getWinnigMoves $ NEArray.mapMaybe fn wps) boardState.history 

    hasWinner :: Square
    hasWinner 
      | NEArray.length boardState.history <= boardSize = Nothing
--      |  =  
      | otherwise = Nothing
    
    mkSquareSlot row colIdx 
      = HH.slot _square (mkPosition row colIdx) (mkSquareComponent (mkPosition row colIdx) boardState) unit 
      <<< handleCell <<< mkPosition row $ colIdx

    mkRow idx = do
      HH.div
        [ HP.class_ $ ClassName "boardRow"
        ]
        $ NEArray.toArray
        $ map (mkSquareSlot idx) sizeArray

    handleCell pos (IsClicked move se) = do
      when (pos == move) do
        newSt <- Hooks.modify boardStateIdx $ updateState se        -- | update state when we have a cell match with next turn
        void <<< log $ "We updated Board State with " -- <> show newSt                                                                
      Hooks.raise rec.outputToken $ HasWinner boardState hasWinner                                                 
    handleCell pos m = do
      void <<< log $ "We could not capture proper message for cell change " <> show pos
      pure unit

  Hooks.pure $ HH.div_ $ NEArray.toArray $ map mkRow sizeArray
  where
  _square :: Proxy "square"
  _square = Proxy
  
--- Helpers ---

updateState
  :: StateElem
  -> BoardState
  -> BoardState
updateState se st =
  { history: se NEArray.: st.history
  , nextTurn: nextPlayer st.nextTurn
  }

mkWinningBoardPositions :: NonEmptyArray (NonEmptyArray Position)
mkWinningBoardPositions 
  =  map (go sizeArray) monoArr
  <> map (\mArr -> go mArr sizeArray) monoArr
  <> NonEmptyArray [ go sizeArray sizeArray
                   , go sizeArray $ NEArray.reverse sizeArray ] 
  where
    go = NEArray.zipWith mkPosition
    monoArr = map (NEArray.replicate boardSize) sizeArray 
