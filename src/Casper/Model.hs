{-# LANGUAGE GADTs #-}
module Casper.Model
  (
    balanceOf
  , setBalance
  , updateBalance
  , allowance
  , setAllowance
  , pushEvent
  , clearEvents
  , runScenarioAction
  , runScenario
  )
where


import Casper.Address
import Casper.CSP
import Casper.Event
import Casper.ScenarioAction
import Casper.TokenState

import Control.Lens hiding (element)
import Control.Lens.Iso (non)


balanceOf :: Address -> TokenState -> CSP
balanceOf address state =
  view balances state ^.at address . non 0


setBalance :: Address -> CSP -> TokenState -> TokenState
setBalance address value =
  over balances (at address ?~ value)


updateBalance :: Address -> (CSP -> CSP) -> TokenState -> TokenState
updateBalance address updater state  =
  setBalance address (updater $ balanceOf address state) state


allowance :: Address -> Address -> TokenState -> CSP
allowance from to state =
  view allowed state ^.at from . non mempty ^.at to . non 0

setAllowance :: Address -> Address -> CSP -> TokenState -> TokenState
setAllowance from to value =
  over allowed (at from . non mempty . at to . non 0 .~ value)


updateAllowance :: Address -> Address -> (CSP -> CSP) -> TokenState -> TokenState
updateAllowance from to updater state =
  setAllowance from to (updater $ allowance from to state) state


pushEvent :: Event -> TokenState -> TokenState
pushEvent event = over events (event:) . over logs (event:)


clearEvents :: TokenState -> TokenState
clearEvents = set events []


-- | Oracle for Casper token.
--
-- This code was written without looking into Casper SC source code.
runScenarioAction :: TokenState -> ScenarioAction -> TokenState
runScenarioAction state (Transfer sender receiver value)
  | balanceOf sender state >= value &&
    value > 0 =
      updateBalance receiver (+ value)  .
      updateBalance sender (flip (-) value) .
      pushEvent (TransferEvent sender receiver value) $
      clearEvents state
  | otherwise = clearEvents state
runScenarioAction state (TransferFrom sender payer receiver value)
  | allowance payer sender state >= value &&
    value > 0 &&
    balanceOf payer state >= value =
      updateBalance receiver (+ value)  .
      updateBalance payer (flip (-) value) .
      updateAllowance payer sender (flip (-) value) .
      pushEvent (TransferEvent payer receiver value) $
      clearEvents state
  | otherwise = clearEvents state
runScenarioAction state (Approve sender receiver value) =
    setAllowance sender receiver value .
    pushEvent (ApprovalEvent sender receiver value) $
    clearEvents state
runScenarioAction state (ScenarioCons a b) =
  runScenarioAction (runScenarioAction state a) b


runScenario :: TokenState -> Scenario -> TokenState
runScenario = foldl runScenarioAction
