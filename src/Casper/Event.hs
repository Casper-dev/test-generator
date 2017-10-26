module Casper.Event
  (
    Event (..)
  )
where


import Casper.Address
import Casper.CSP


-- | Represents a single event fired while executing scenario.
data Event =
  TransferEvent Address Address CSP |
  ApprovalEvent Address Address CSP
  deriving (Eq)


instance Show Event where
  show (TransferEvent a b v) =
    "['Transfer', " ++ show a ++ ", " ++ show b ++ ", " ++ num v ++ "]"
  show (ApprovalEvent a b v) =
    "['Approval', " ++ show a ++ ", " ++ show b ++ ", " ++ num v ++ "]"
