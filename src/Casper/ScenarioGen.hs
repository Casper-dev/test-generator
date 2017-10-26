module Casper.ScenarioGen
  (
    makeScenarios
  , makeScenariosWith
  , withTransfer
  , withApprove
  , withTransferFrom
  )
where


import Casper.Address
import Casper.CSP
import Casper.ScenarioAction


makeScenarios :: [Address] -> [CSP] -> Int -> [Scenario]
makeScenarios = makeScenariosWith [withTransfer, withApprove, withTransferFrom]


makeScenariosWith ::
  [([Address] -> [CSP] -> [ScenarioAction])] -- ^ List of functions that generate 'ScenarioAction's
                                           -- (e.g. 'withTransfer', 'withApprove', 'withTransferFrom')
  -> [Address] -- ^ List of possible participants
  -> [CSP] -- ^ List of possible token count values.
  -> Int -- ^ Recursion depth limit.
  -> [Scenario]
makeScenariosWith [] _ _ _ = []
makeScenariosWith _ [] _ _ = []
makeScenariosWith _ _ _ 0 = []
makeScenariosWith functions users values depth =
  concatMap (\action ->
                [[action]] ++ map (\scenario -> action : scenario)
                (makeScenariosWith functions users values (depth - 1))) actions
  where
    actions = foldl (++) [] $ map (\f -> f users values) functions


withTransfer ::
  [Address] -- ^ List of possible participants
  -> [CSP] -- ^ List of possible token count values.
  -> [ScenarioAction]
withTransfer [] _ = []
withTransfer _ [] = []
withTransfer users values =
  concatMap (\sender ->
    concatMap (\receiver -> map (transfer sender receiver) values)
      users)
    users


withApprove ::
  [Address] -- ^ List of possible participants
  -> [CSP] -- ^ List of possible token count values.
  -> [ScenarioAction]
withApprove [] _ = []
withApprove _ [] = []
withApprove users values =
  concatMap (\sender ->
    concatMap (\receiver ->
      map (approve sender receiver) values)
      users)
    users


withTransferFrom ::
  [Address] -- ^ List of possible participants
  -> [CSP] -- ^ List of possible token count values.
  -> [ScenarioAction]
withTransferFrom [] _ = []
withTransferFrom users values =
  concatMap (\sender ->
    concatMap (\payer ->
      concatMap (\receiver ->
        map (transferFrom sender payer receiver) values)
        users)
      users)
    users
