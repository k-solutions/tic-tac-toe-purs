module Components.Game
  ( mkGameComponent
  , calculateWinner
  ) where

import Prelude

import Components.Board (nextPlayer, mkBoardComponent)
import Components.Square (GameState (..), MoveCount (..), Message(..), Player(..), Position(..), Square, StateElem(..), emptyStateElem, mkPosition)
import Control.Alt ((<|>))
import Data.Array as Array
import Data.Array.NonEmpty as NEArray
import Data.Foldable (or)
import Data.Maybe (Maybe(..))
-- import Data.Semigroup.Foldable (foldr1)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Data.Unfoldable1 (unfoldr1)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
-- import Halogen.HTML.Core (HTML(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Type.Proxy (Proxy(..))
-- import Web.DOM (Comment)
import Web.HTML.Common (ClassName(..))

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
  gameSt@(gameState /\ gameStateIdx) <- Hooks.useState initState
  let
    moves = Array.fromFoldable $ NEArray.mapWithIndex (\ix _ -> setSlot ix) gameState.history
      where
      setSlot ix = HH.slot _history ix (mkHistoryButton ("Move # " <> show ix) ix) unit handleHistory

    currentMove = NEArray.head gameState.history

    handleHistory (IsReturned ix) = do
      Hooks.modify_ gameStateIdx (\st -> st { currentMove = MkMoveCount ix, nextTurn = skipTurnsBy st.nextTurn ix })
    handleHistory _ = pure unit

    status = case calculateWinner currentMove of
      Just winner -> "Winner is : " <> show winner
      Nothing -> "Next move is: " <> show gameState.nextTurn -- | We need to get previous state                                                                    
  Hooks.pure do
    HH.div
      [ HP.class_ $ ClassName "game"
      ]
      [ HH.slot _board unit (mkBoardComponent gameSt) unit absurd
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

  skipTurnsBy :: Player -> Int -> Player
  skipTurnsBy p i = formPipe p
    where
    formPipe = NEArray.foldr1 (<<<) <<< unfoldr1 go $ i
    go i'
      | i' <= 0 = Tuple nextPlayer Nothing
      | otherwise = Tuple nextPlayer $ Just (i' - 1)

  initState :: GameState
  initState =
    { history: NEArray.singleton emptyStateElem
    , currentMove: MkMoveCount 0
    , nextTurn: X
    }

--- Helpers --- 

calculateWinner :: StateElem -> Square
calculateWinner f =
  let
    winByPlayer :: Player -> Square
    winByPlayer p =
      if
        -- Check for a row full of player 'p'
        [ [ 0, 1, 2 ] <#> \i -> [ 0, 1, 2 ] <#> \j -> f $ mkPosition i j
        -- Check for a full column
        , [ 0, 1, 2 ] <#> \j -> [ 0, 1, 2 ] <#> \i -> f $ mkPosition i j
        -- Check diagonals
        , [ [ 0, 1, 2 ] <#> \k -> f $ mkPosition k k ]
        , [ [ 0, 1, 2 ] <#> \k -> f $ mkPosition k (2 - k) ]
        ]
          # Array.concat
              >>> map (Array.all (_ == Just p))
              >>> or then
        Just p
      else
        Nothing
  in
    winByPlayer X <|> winByPlayer O

--gameWinner :: StateElem -> Square
--gameWinner mvFun = do
--  let
--    isFull = all (_ == Just p)   
--    allPositions = []  
--    go :: Player -> Square
--    go p  
--      | any isFull allPositions = Just p 
--      | otherwise = Nothing      
--   
--  go X <|> go O      
--  

