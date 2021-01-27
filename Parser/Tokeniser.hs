{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE StandaloneDeriving #-}
{-|
Description: A simple state- and transition-based tokeniser with location tracking

* Tokeniser
* A data structure representing a sequence of tokens, for internal use in the parser
-}
module Parser.Tokeniser (
  Tokeniser',
  Tokens',
  add_token,
  current_line_and_char,
  delete_char,
  gather_token,
  get_char,
  get_line_and_char,
  get_token,
  take_token,
  tokenisation_error,
  tokenise,
  tokens_ended) where
  import Control.Monad.Except (MonadError (..))
  import Control.Monad.RWS.Strict (RWST, execRWST)
  import Control.Monad.Reader (MonadReader (..))
  import Control.Monad.State.Strict (MonadState (..))
  import Control.Monad.Writer.Strict (MonadWriter (..))
  import Parser.Errors (Error (..))
  import Parser.Line_and_char (L (..), Line_and_char, init_line_and_char)
  data State char = State {state_line_and_char :: Line_and_char, state_text :: [char]}
  -- | A tokeniser that works with any kind of custom characters, tokens and errors. The custom character type is necessary
  -- if you want to classify characters according to their behavior before tokenisation - for example, wrap all operators,
  -- letters, delimiters or digits in the same constructor to simplify pattern matching.
  data Tokeniser' char token err t =
    Tokeniser {run_tokeniser :: RWST (char -> Line_and_char -> Line_and_char) [L token] (State char) (Either err) t}
  -- | A sequence of tokens with locations. For internal use in the parser.
  data Tokens' token = Tokens [L token] Line_and_char
  instance Applicative (Tokeniser' char token err) where
    Tokeniser tokenise_0 <*> Tokeniser tokenise_1 = Tokeniser (tokenise_0 <*> tokenise_1)
    pure x = Tokeniser (return x)
  instance Functor (Tokeniser' char token err) where
    fmap f (Tokeniser tokenise') = Tokeniser (f <$> tokenise')
  instance Monad (Tokeniser' char token err) where
    Tokeniser tokenise' >>= f = Tokeniser (tokenise' >>= run_tokeniser <$> f)
  deriving instance Show char => Show (State char)
  deriving instance Show token => Show (Tokens' token)
  -- | Add the token to the output. Note that the order of adding tokens is important.
  add_token :: Line_and_char -> token -> Tokeniser' char token err ()
  add_token line_and_char token = tell_Tokeniser [L line_and_char token]
  ask_Tokeniser :: Tokeniser' char token err (char -> Line_and_char -> Line_and_char)
  ask_Tokeniser = Tokeniser ask
  -- | Get the location of the first token or, if there are none, the end of file. For internal use in the parser.
  current_line_and_char :: Tokens' token -> Line_and_char
  current_line_and_char (Tokens tokens end_line_and_char) =
    case tokens of
      [] -> end_line_and_char
      L line_and_char _ : _ -> line_and_char
  -- | Delete the first character from the remaining text. Automatically updates the location.
  delete_char :: Tokeniser' char token err ()
  delete_char =
    do
      next_line_and_char <- ask_Tokeniser
      State line_and_char text <- get_Tokeniser
      case text of
        [] -> return ()
        char : text' -> put_Tokeniser (State (next_line_and_char char line_and_char) text')
  -- | Add a token that consists of several characters - for example, an operator, a word or a natural number. You have to
  -- provide a function that recognises suitable characters and a function that transforms the resulting string into a token.
  gather_token :: (char -> Maybe Char) -> (String -> token) -> Tokeniser' char token err ()
  gather_token recognise_char string_to_token =
    do
      line_and_char <- get_line_and_char
      token <- gather_token' recognise_char
      add_token line_and_char (string_to_token token)
  gather_token' :: (char -> Maybe Char) -> Tokeniser' char token err String
  gather_token' recognise_char =
    let
      f = gather_token' recognise_char
    in
      do
        maybe_char <- get_char 0
        case maybe_char >>= recognise_char of
          Nothing -> return ""
          Just char ->
            do
              delete_char
              token <- f
              return (char : token)
  get_Tokeniser :: Tokeniser' char token err (State char)
  get_Tokeniser = Tokeniser get
  -- | Take a look at a character without deleting it. Returns @Nothing@ if the index is negative or if the remaining text is
  -- too short.
  get_char :: Int -> Tokeniser' char token err (Maybe char)
  get_char i =
    do
      text <- state_text <$> get_Tokeniser
      return
        (case 0 <= i && i < length text of
          False -> Nothing
          True -> Just (text !! i))
  -- | Get the current location of the tokeniser.
  get_line_and_char :: Tokeniser' char token err Line_and_char
  get_line_and_char = state_line_and_char <$> get_Tokeniser
  -- | Get the first token without deleting it. For internal use in the parser.
  get_token :: Tokens' token -> Maybe token
  get_token (Tokens tokens _) =
    case tokens of
      [] -> Nothing
      L _ token : _ -> Just token
  put_Tokeniser :: State char -> Tokeniser' char token err ()
  put_Tokeniser st = Tokeniser (put st)
  -- | Recognises tokens that fit a certain pattern and transforms them into something more useful - for example, a string or an
  -- integer. Returns @Nothing@ if the first token does not fit the pattern, and returns the transformed token and the rest of
  -- the sequence if it does fit. For internal use in the parser.
  take_token :: (token -> Maybe t) -> Tokens' token -> Maybe (t, Tokens' token)
  take_token f (Tokens tokens end_line_and_char) =
    case tokens of
      [] -> Nothing
      L _ token : tokens' ->
        do
          x <- f token
          return (x, Tokens tokens' end_line_and_char)
  tell_Tokeniser :: [L token] -> Tokeniser' char token err ()
  tell_Tokeniser tokens = Tokeniser (tell tokens)
  throwError_Tokeniser :: err -> Tokeniser' char token err t
  throwError_Tokeniser err = Tokeniser (throwError err)
  -- | Throw a tokenisation error at the current location.
  tokenisation_error :: (Line_and_char -> err) -> Tokeniser' char token err t
  tokenisation_error err =
    do
      line_and_char <- get_line_and_char
      throwError_Tokeniser (err line_and_char)
  -- | Tokenise the text. For internal use in the parser.
  tokenise ::
    (
      (Char -> char) ->
      (char -> Line_and_char -> Line_and_char) ->
      Tokeniser' char token err () ->
      String ->
      Either Error (Either err (Tokens' token)))
  tokenise classify_char next_line_and_char (Tokeniser tokenise') text =
    case execRWST tokenise' next_line_and_char (State init_line_and_char (classify_char <$> text)) of
      Left err -> Right (Left err)
      Right (State line_and_char text', tokens) ->
        case text' of
          [] -> Right (Right (Tokens tokens line_and_char))
          _ -> Left Incomplete_tokenisation
  -- | Check whether the sequence of tokens has ended. For internal use in the parser.
  tokens_ended :: Tokens' token -> Bool
  tokens_ended (Tokens tokens _) = null tokens