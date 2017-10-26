{-# LANGUAGE TemplateHaskell #-}
module Casper.TokenState
  (
    TokenState (..)
  , initialState

  , balances
  , allowed
  , events
  , logs
  , participants
  )
where


import Casper.Address
import Casper.CSP
import Casper.Event

import Control.Lens hiding (element)
import Data.List (intersperse)
import qualified Data.Map as Map


data TokenState =
  TokenState { _balances :: Map.Map Address CSP
             , _allowed :: Map.Map Address (Map.Map Address CSP)
             , _events :: [Event] -- ^ Events emitted while performing last 'ScenarioAction'
             , _logs :: [Event] -- ^ Events emitted while performing 'Scenario'.
             , _participants :: [Address] -- ^ Users who participated in current scenario (should be set by 'scenarioToJS')
             }
  deriving (Eq)


makeLenses ''TokenState


instance Show TokenState where
  show state =
    "balances:\n" ++ concat (intersperse "\n" $
                             map (\(k, v) -> "  " ++ show k ++ ": " ++ show v) $
                             Map.toList $ view balances state) ++ "\n" ++
    "allowed:\n" ++ (Map.foldlWithKey (\b k v -> b ++ "  " ++
                                        show k ++ ":\n" ++
                                        (Map.foldlWithKey (\b' k' csp -> b' ++ "    " ++
                                                            show k' ++ ": " ++
                                                            show csp ++ "\n") "" v)) "" $ view allowed state)


-- | Contract state right after issuance.
initialState = TokenState {
  _balances = Map.fromList [ (me, 10000000) ]
  , _allowed = Map.fromList []
  , _events = []
  , _logs = []
  , _participants = []
  }
