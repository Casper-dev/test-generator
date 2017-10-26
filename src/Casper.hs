-- | This module contains a model (oracle) and a test generator for the Casper token.
--
-- The technique used here for tests is known as "model based testing".
module Casper
  (
    CSP
  , Address (..)
  , Event (..)
  , ScenarioAction
  , Scenario
  , TokenState

  , flatten

  , initialState
  , balanceOf
  , setBalance
  , updateBalance
  , allowance
  , setAllowance
  , getParticipants
  , pushEvent
  , clearEvents
  , runScenarioAction
  , runScenario

  , JSWrapper
  , line, indent
  , scenarioToJS
  , pushEventsToLog
  , actionsToJS

  , transfer
  , approve
  , transferFrom

  , makeScenarios
  , makeScenariosWith
  , withTransfer, withApprove, withTransferFrom
  , me, a, b, c, d, e, f, g, h
  )
where

import Casper.Address
import Casper.CSP
import Casper.CodeGen
import Casper.Event
import Casper.JSWrapper
import Casper.Model
import Casper.ScenarioAction
import Casper.ScenarioGen
import Casper.TokenState
