{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE StandaloneDeriving #-}
{-|
Description: Errors for the tokeniser and the parser

* Errors for the tokeniser and the parser
-}
module Parser.Errors (Error (..)) where
  -- | Tokenisation and parsing errors that indicate not an issue with the text but a user error.
  data Error = Ambiguity | Incomplete_tokenisation
  deriving instance Show Error