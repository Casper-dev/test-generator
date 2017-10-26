module Casper.CSP
  (
    CSP
  , num
  , nums
  )
where


import Data.List (intersperse)


type CSP = Integer


num :: CSP -> String
num n
  | n < maxSafeInteger = show n
  | otherwise = "new BigNumber('" ++ show n ++ "')"


nums :: [CSP] -> String
nums l =
  "[" ++ (concat . intersperse "," $ map num l) ++ "]"


-- | Number.MAX_SAFE_INTEGER in node.js
maxSafeInteger = 9007199254740991
