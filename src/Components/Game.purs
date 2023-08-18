module Components.Game
  ( mkGameComponent
--  , calculateWinner
  ) where

import Prelude

import Components.Board (mkBoardComponent)
import Control.Alt ((<|>))
import Data.Array as Array
import Data.Array.NonEmpty as NEArray
import Data.Foldable (or)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Data.Unfoldable1 (unfoldr1)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Foreign.Index (ix)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Type.Proxy (Proxy(..))
import Types (Message(..), MoveCount, Player(..), Square, StateElem, BoardState, emptyStateElem, initMoveCount, initPlayer, mkPosition, nextPlayer, reverseMoveCount, unMoveCount, rewindMoveCount)
import Web.HTML.Common (ClassName(..))

--- Types ---

type GameState =
  { boardState  :: BoardState              -- | board state 
  , currentMove :: MoveCount               -- | moves counter 
  , winner      :: Square                  -- | which Player is winner
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
mkGameComponent = Hooks.component $ \_rec _ -> Hooks.do
  gameSt@(gameState /\ gameStateIdx) <- Hooks.useState initState
  let
    moves = Array.fromFoldable $ NEArray.mapWithIndex (\ix _ -> setSlot ix) gameState.boardState.history
      where
      setSlot ix = HH.slot _history ix (mkHistoryButton ("Move # " <> show ix) ix) unit handleHistory

    currentMove = NEArray.head gameState.boardState.history

    rewindState :: Int -> GameState -> Maybe GameState
    rewindState ix st = do 
      rMove <- rewindMoveCount ix st.currentMove
      bState <- rewindBoardState ix st.boardState 
      pure $ 
        { boardState: bState
        , currentMove: rMove 
        , winner:  Nothing       
        }
 
    handleHistory (IsReturned ix) = do
         oldSt <- Hooks.get gameStateIdx
         case rewindState ix oldSt of
           Just st -> Hooks.put gameStateIdx st
           Nothing -> do 
             void $ log $ "Could not rewind state " <> show gameState
             pure unit 
    handleHistory m = do
      void <<< log $ "wrong message captured" <> show m 
      pure unit
   
    handleBoard (HasWinner brdSt sq) = do
      Hook.modify_ gameStateIdx $ \ st -> 
        st { boardState = brdSt
           , currentMove = 
           }       


    status :: String
    status = 
      case gameState.winner of
        Just winner -> "Winner is : " <> show winner
        Nothing -> "Next move is: " <> show gameState.boardState.nextTurn -- | We need to get previous state                                                                    
  Hooks.pure do
    HH.div
      [ HP.class_ $ ClassName "game"
      ]
      [ HH.slot _board unit (mkBoardComponent gameState.boardState) unit handleBoard 
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

  initBoard :: BoardState
  initBoard = 
    { history: NEArray.singleton emptyStateElem
    , nextTurn: initPlayer 
    }

  initState :: GameState
  initState =
    { boardState: initBoard
    , currentMove: initMoveCount
    , winner: Nothing
    }

--- Helpers --- 

rewindBoardState :: Int -> BoardState -> Maybe BoardState
rewindBoardState ix st = do
  rewindHistory <- NEArray.fromArray $ NEArray.drop ix st.history
  pure $ st { history = rewindHistory, nextTurn = reverseTurns st.nextTurn }
  where
    reverseTurns = NEArray.foldr1 (\ cFn accFn -> accFn <<< cFn) 
                 $ NEArray.replicate ix nextPlayer

--calculateWinner :: StateElem -> Square
--calculateWinner f =
--  let
--    winByPlayer :: Player -> Square
--    winByPlayer p =
--      if
--        -- Check for a row full of player 'p'
--        [ [ 0, 1, 2 ] <#> \i -> [ 0, 1, 2 ] <#> \j -> f $ mkPosition i j
--        -- Check for a full column
--        , [ 0, 1, 2 ] <#> \j -> [ 0, 1, 2 ] <#> \i -> f $ mkPosition i j
--        -- Check diagonals
--        , [ [ 0, 1, 2 ] <#> \k -> f $ mkPosition k k ]
--        , [ [ 0, 1, 2 ] <#> \k -> f $ mkPosition k (2 - k) ]
--        ]
--          # Array.concat
--              >>> map (Array.all (_ == Just p))
--              >>> or then
--        Just p
--      else
--        Nothing
--  in
--    winByPlayer X <|> winByPlayer O

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

