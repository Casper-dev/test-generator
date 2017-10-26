{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
module Casper.ScenarioAction
  (
    ScenarioAction
  , ScenarioAction_ (..)
  , Scenario

  , flatten
  , getParticipants

  , transfer
  , approve
  , transferFrom
  )
where


import Casper.Address
import Casper.CSP

import Data.List (nub)


-- | Type parameter is used to satisfy 'Monad' rules (Monad must be of kind * -> *)
data ScenarioAction_ a where
  Transfer :: Address -> Address -> CSP -> ScenarioAction_ ()                  -- ^ Sender, To, Value
  TransferFrom :: Address -> Address ->  Address ->  CSP -> ScenarioAction_ () -- ^ Sender, From, To, Value
  Approve :: Address -> Address -> CSP -> ScenarioAction_ ()               -- ^ Sender, Spender, Value
  ScenarioCons :: ScenarioAction_ () -> ScenarioAction_ () -> ScenarioAction_ ()


deriving instance Eq (ScenarioAction_ ())


-- It's here just for do-notation
instance Functor ScenarioAction_ where
  fmap = undefined


-- It's here just for do-notation
instance Applicative ScenarioAction_ where
  (<*>) = undefined
  pure = undefined


-- It's here just for do-notation
instance Monad ScenarioAction_ where
  (>>=) = undefined
  x@(Transfer _ _ _) >> y@(Transfer _ _ _) = ScenarioCons x y
  x@(Transfer _ _ _) >> y@(TransferFrom _ _ _ _) = ScenarioCons x y
  x@(Transfer _ _ _) >> y@(Approve _ _ _) = ScenarioCons x y
  x@(Transfer _ _ _) >> y@(ScenarioCons _ _) = ScenarioCons x y
  x@(TransferFrom _ _ _ _) >> y@(Transfer _ _ _) = ScenarioCons x y
  x@(TransferFrom _ _ _ _) >> y@(TransferFrom _ _ _ _) = ScenarioCons x y
  x@(TransferFrom _ _ _ _) >> y@(Approve _ _ _) = ScenarioCons x y
  x@(TransferFrom _ _ _ _) >> y@(ScenarioCons _ _) = ScenarioCons x y
  x@(Approve _ _ _) >> y@(Transfer _ _ _) = ScenarioCons x y
  x@(Approve _ _ _) >> y@(TransferFrom _ _ _ _) = ScenarioCons x y
  x@(Approve _ _ _) >> y@(Approve _ _ _) = ScenarioCons x y
  x@(Approve _ _ _) >> y@(ScenarioCons _ _) = ScenarioCons x y
  x@(ScenarioCons _ _) >> y@(Transfer _ _ _) = ScenarioCons x y
  x@(ScenarioCons _ _) >> y@(TransferFrom _ _ _ _) = ScenarioCons x y
  x@(ScenarioCons _ _) >> y@(Approve _ _ _) = ScenarioCons x y
  x@(ScenarioCons _ _) >> y@(ScenarioCons _ _) = ScenarioCons x y


-- Hide type parameter
type ScenarioAction = ScenarioAction_ ()


type Scenario = [ScenarioAction]


instance {-# OVERLAPPABLE #-} Show ScenarioAction where
  show (Transfer from to value) =
    "transfer" ++ " " ++ show from ++ " " ++ show to ++ " " ++ show value
  show (TransferFrom sender spender receiver value) =
    "transferFrom" ++ " " ++ show sender ++ " " ++ show spender ++ " " ++ show receiver ++ " " ++ show value
  show (Approve from to value) =
    "approve" ++ " " ++ show from ++ " " ++ show to ++ " " ++ show value
  show (ScenarioCons a b) =
    show a ++ "\n" ++ show b


instance {-# OVERLAPPING #-} Show Scenario where
  show [] = ""
  show s = show $ foldl1 ScenarioCons s


-- | Convert binary tree ('ScenarioAction') to unidimensional list.
flatten :: ScenarioAction -> [ScenarioAction]
flatten (ScenarioCons a b) = flatten a ++ flatten b
flatten x = [x]


getParticipants :: [ScenarioAction] -> [Address]
getParticipants l = nub $ concatMap getParticipants' l
  where
    getParticipants' :: ScenarioAction -> [Address]
    getParticipants' (Transfer a b _) = [a, b]
    getParticipants' (TransferFrom a b c _) = [a, b, c]
    getParticipants' (Approve a b _) = [a, b]
    getParticipants' (ScenarioCons a b) = getParticipants' a ++ getParticipants' b


-- Functions used to form a DSL - as for now, they're just trivial wrappers
-- over 'ScenarioAction_' constructors.


-- | ERC20 `transfer` function call.
transfer ::
  Address -- ^ Transaction sender and also spender
  -> Address -- ^ Receiver
  -> CSP -- ^ Tokens
  -> ScenarioAction
transfer = Transfer

-- | ERC20 `transferFrom` function call.
transferFrom ::
  Address  -- ^ Transaction sender
  -> Address -- ^ Spender
  -> Address -- ^ Receiver
  -> CSP -- ^ Tokens
  -> ScenarioAction
transferFrom = TransferFrom

-- | ERC20 `approve` function call.
approve ::
  Address -- ^ Transaction sender and also spender
  -> Address -- ^ Receiver
  -> CSP -- ^ Tokens
  -> ScenarioAction
approve = Approve
