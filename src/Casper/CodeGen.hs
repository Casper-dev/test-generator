{-# LANGUAGE GADTs #-}
module Casper.CodeGen
  (
    scenarioToJS
  , pushEventsToLog
  , actionsToJS
  , toAssertions
  )
where

import Casper.JSWrapper
import Casper.Model
import Casper.ScenarioAction
import Casper.TokenState

import Control.Lens hiding (element)
import Data.List (intersperse)
import Data.Monoid ((<>))


describe :: Scenario -> String
describe = show . concatMap (("\n      " ++) . show)


scenarioToJS :: TokenState -> Scenario -> JSWrapper
scenarioToJS state scenario =
  let state' = set participants (getParticipants scenario) state in
  indent $
  line ("it(" ++ describe scenario ++  ", () => {") <>
  indent (line "return Casper.new().then(casper => {" <>
          indent (line ("var utils = new TestUtils(web3, casper, " ++
                        (show $ view participants state') ++ ");") <>
                  actionsToJS state' scenario) <>
          line "});") <>
  line "});"


pushEventsToLog :: JSWrapper
pushEventsToLog = line ("utils.saveEvents(result);")


actionsToJS :: TokenState -> Scenario -> JSWrapper
actionsToJS state [] = toAssertions state JSNil
actionsToJS state (x@(Transfer sender receiver value) : xs) =
  let newState = runScenarioAction state x in
  line ("return casper.transfer(" ++ show receiver ++ ", " ++
         show value ++
         ", { from: " ++ show sender ++ " }).then(result => {") <>
  indent (pushEventsToLog <>
         (toAssertions newState $ actionsToJS newState xs)) <>
  line "});"
actionsToJS state (x@(TransferFrom sender payer receiver value) : xs) =
  let newState = runScenarioAction state x in
  line ("return casper.transferFrom(" ++ (show payer ++ ", " ++
                                          show receiver ++ ", " ++
                                          show value ++ ", { from: " ++
                                          show sender ++ " }).then(result => {")) <>
  indent (pushEventsToLog <>
         (toAssertions newState $ actionsToJS newState xs)) <>
  line "});"
actionsToJS state (x@(Approve sender receiver value) : xs) =
  let newState = runScenarioAction state x in
  line ("return casper.approve(" ++ (show receiver ++ ", " ++
                                     show value ++ ", { from: " ++ show sender ++ " }).then(result => {")) <>
  indent (pushEventsToLog <>
          (toAssertions newState $ actionsToJS newState xs)) <>
  line "});"


toAssertions :: TokenState -> JSWrapper -> JSWrapper
toAssertions state JSNil = -- final assertions, check logs
  (line ("utils.assertLogs(" ++ show (reverse $ view logs state) ++ ");"))
toAssertions state code =
  line ("return utils.assertState(" ++
         show balances_ ++ ", " ++ show allowances ++ ").then(() => {") <>
  indent code <>
  line "});"
  where
    accounts = view participants state
    balances_ = map (\account -> view balances state ^.at account . non 0) accounts
    allowances = map (\payer ->
                        map (\receiver ->
                               view allowed state
                               ^. at payer . non mempty
                               ^. at receiver . non 0) accounts) accounts
