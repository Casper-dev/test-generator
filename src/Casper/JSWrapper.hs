module Casper.JSWrapper
  (
    JSWrapper (JSNil),
    line, indent
  )
where


import Data.List (intersperse)


-- | A wrapper over JS code - used for pretty printing.
data JSWrapper =
  Indent JSWrapper -- ^ Indentation
  | Line String -- ^ Some code
  | JSNil -- ^ No code
  | Lines JSWrapper JSWrapper -- ^ One more constructor to satisfy Monoid rules.


instance Show JSWrapper where
  show (Indent w) = concat . intersperse "\n" . map ("  " ++) . lines $ show w
  show (Line str) = str
  show JSNil = ""
  show (Lines a b) = show a ++ "\n" ++ show b


instance Monoid JSWrapper where
  mempty = JSNil
  mappend = Lines


line = Line


indent = Indent
