module Casper.Address
  (
    Address (..)
  , me, a, b, c, d, e, f, g, h
  )
where


data Address =
  Contract          -- ^ Casper SC itself
  | Account Integer -- ^ Normal user (no real addresses are used in tests - we check only logic here)
  deriving (Eq, Ord)


instance Show Address where
  show Contract = "casper"
  show (Account 0) = "me"
  show (Account i)
    | i <= 26 = ["abcdefghijklmnopqrstuvwxyz" !! (fromInteger i - 1)]
    | otherwise = "accounts[" ++ show i ++ "]"


me = (Account 0)
a = (Account 1)
b = (Account 2)
c = (Account 3)
d = (Account 4)
e = (Account 5)
f = (Account 6)
g = (Account 7)
h = (Account 8)
