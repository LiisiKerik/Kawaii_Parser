{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE StandaloneDeriving #-}
{-|
Description: An unambiguous parser with symmetric choice and location tracking

* Parser
* Parsing locations
* Parsing brackets
* Parsing lists
-}
module Parser.Parser (
  (<+>),
  Parse_error' (..),
  Parser',
  add_location,
  parse,
  parse_brackets,
  parse_empty_list,
  parse_line_and_char,
  parse_list,
  parse_many,
  parse_non_empty_list,
  parse_some,
  parse_token,
  parse_token') where
  import Control.Monad.Except (ExceptT (..), MonadError (..), runExceptT, withExceptT)
  import Control.Monad.State.Strict (MonadState (..), StateT (..), evalStateT, modify)
  import Data.Set (Set, empty, singleton, union)
  import Parser.Errors (Error (..))
  import Parser.Line_and_char (L (..), Line_and_char)
  import Parser.Tokeniser (Tokeniser', Tokens', current_line_and_char, get_token, take_token, tokenise, tokens_ended)
  -- | The types of tokens that were expected at a certain location and the token that was actually found instead (or @Nothing@
  -- if the end of file was reached unexpectedly).
  data Parse_error' token = Parse_error Line_and_char (Set String) (Maybe token)
  -- | A parser that works with any kind of tokens.
  newtype Parser' token t = Parser {run_parser :: StateT (State token) (ExceptT (Parse_error' token) (Either Error)) t}
  data State token = State {state_tokens :: Tokens' token, state_error :: Parse_error' token}
  -- | Symmetric choice between two parsers that selects the longest match. Note that if both parsers successfully reach the
  -- same location it will result in an ambiguity error that, unlike a normal parse error, is not recoverable by backtracking.
  -- Also note that while this operator is normally associative it is not when ambiguity errors are involved.
  infixr 3 <+>
  (<+>) :: Eq token => Parser' token t -> Parser' token t -> Parser' token t
  Parser (StateT parse_0) <+> Parser (StateT parse_1) =
    Parser
      (StateT
        (\st ->
          ExceptT
            (do
              (err_0, result_0) <- deconstruct_result <$> runExceptT (parse_0 st)
              (err_1, result_1) <- deconstruct_result <$> runExceptT (parse_1 st)
              (
                construct_result (add_errors err_0 err_1) <$>
                case (result_0, result_1) of
                  (Nothing, Nothing) -> Right Nothing
                  (Nothing, Just _) -> Right result_1
                  (Just _, Nothing) -> Right result_0
                  (Just (_, tokens_0), Just (_, tokens_1)) ->
                    case compare (current_line_and_char tokens_0) (current_line_and_char tokens_1) of
                      LT -> Right result_1
                      EQ -> Left Ambiguity
                      GT -> Right result_0))))
  infixr 4 ===
  (===) :: Eq t => t -> t -> t
  x === y =
    case x == y of
      False -> undefined
      True -> x
  instance Applicative (Parser' token) where
    Parser parse_0 <*> Parser parse_1 = Parser (parse_0 <*> parse_1)
    pure x = Parser (return x)
  instance Functor (Parser' token) where
    fmap f (Parser parse') = Parser (f <$> parse')
  instance Monad (Parser' token) where
    Parser parse' >>= f = Parser (parse' >>= run_parser <$> f)
  deriving instance Show token => Show (Parse_error' token)
  deriving instance Show token => Show (State token)
  -- | Parse something with an added location from the first token.
  add_location :: Parser' token t -> Parser' token (L t)
  add_location parse_t = L <$> parse_line_and_char <*> parse_t
  add_errors :: Eq token => Parse_error' token -> Parse_error' token -> Parse_error' token
  add_errors
    lookahead_0 @ (Parse_error line_and_char_0 expected_0 found_token_0)
    lookahead_1 @ (Parse_error line_and_char_1 expected_1 found_token_1) =
      case compare line_and_char_0 line_and_char_1 of
        LT -> lookahead_1
        EQ -> Parse_error (line_and_char_0 === line_and_char_1) (union expected_0 expected_1) (found_token_0 === found_token_1)
        GT -> lookahead_0
  construct_result :: Parse_error' token -> Maybe (t, Tokens' token) -> Either (Parse_error' token) (t, State token)
  construct_result err result =
    case result of
      Nothing -> Left err
      Just (x, tokens) -> Right (x, State tokens err)
  deconstruct_result :: Either (Parse_error' token) (t, State token) -> (Parse_error' token, Maybe (t, Tokens' token))
  deconstruct_result result =
    case result of
      Left err -> (err, Nothing)
      Right (x, State tokens err) -> (err, Just (x, tokens))
  get_Parser :: Parser' token (State token)
  get_Parser = Parser get
  modify_Parser :: (State token -> State token) -> Parser' token ()
  modify_Parser f = Parser (modify f)
  -- | Parse the text. You have to provide a function that classifies characters, a function that tells how to update the
  -- location depending on the character, a tokeniser, a parser and a function that converts parse errors to your preferred
  -- type.
  parse ::
    (
      Eq token => 
      (Char -> char) ->
      (char -> Line_and_char -> Line_and_char) ->
      Tokeniser' char token err () ->
      Parser' token t ->
      (Parse_error' token -> err) ->
      String ->
      Either Error (Either err t))
  parse classify_char next_line_and_char tokenise' parse' transform_parse_error text =
    runExceptT
      (do
        tokens <- ExceptT (tokenise classify_char next_line_and_char tokenise' text)
        withExceptT
          transform_parse_error
          (evalStateT
            (run_parser (parse_end parse'))
            (State tokens (Parse_error (current_line_and_char tokens) empty (get_token tokens)))))
  -- | Parse a term in brackets.
  parse_brackets :: Parser' token () -> Parser' token () -> Parser' token t -> Parser' token t
  parse_brackets parse_left_bracket parse_right_bracket parse_t =
    do
      parse_left_bracket
      x <- parse_t
      parse_right_bracket
      return x
  parse_certain_token :: Eq token => token -> token -> Maybe ()
  parse_certain_token token token' =
    case token == token' of
      False -> Nothing
      True -> Just ()
  parse_element :: Parser' token () -> Parser' token t -> Parser' token t
  parse_element parse_separator parse_t =
    do
      parse_separator
      parse_t
  -- | Returns an empty list.
  parse_empty_list :: Parser' token [t]
  parse_empty_list = return []
  parse_end :: Eq token => Parser' token t -> Parser' token t
  parse_end parse_t =
    do
      x <- parse_t
      parse_end'
      return x
  parse_end' :: Eq token => Parser' token ()
  parse_end' =
    do
      tokens <- state_tokens <$> get_Parser
      case tokens_ended tokens of
        False -> parse_error "end of text"
        True -> return ()
  parse_error :: Eq token => String -> Parser' token t
  parse_error expected =
    do
      State tokens err <- get_Parser
      throwError_Parser (add_errors err (Parse_error (current_line_and_char tokens) (singleton expected) (get_token tokens)))
  -- | Get the current location.
  parse_line_and_char :: Parser' token Line_and_char
  parse_line_and_char = current_line_and_char <$> state_tokens <$> get_Parser
  -- | Parse a (possibly empty) list with separators.
  parse_list :: Eq token => Parser' token () -> Parser' token t -> Parser' token [t]
  parse_list parse_separator parse_t = parse_empty_list <+> parse_non_empty_list parse_separator parse_t
  -- | Parse a (possibly empty) list without separators.
  parse_many :: Eq token => Parser' token t -> Parser' token [t]
  parse_many parse_t = parse_empty_list <+> parse_some parse_t
  -- | Parse a non-empty list with separators.
  parse_non_empty_list :: Eq token => Parser' token () -> Parser' token t -> Parser' token [t]
  parse_non_empty_list parse_separator parse_t = (:) <$> parse_t <*> parse_many (parse_element parse_separator parse_t)
  -- | Parse a non-empty list without separators.
  parse_some :: Eq token => Parser' token t -> Parser' token [t]
  parse_some parse_t = (:) <$> parse_t <*> parse_many parse_t
  -- | Parse a certain token (for example, a delimiter or a keyword) without returning any results. You also have to provide a
  -- string that briefly describes the kind of token that is expected - it is used to provide detailed parse errors.
  parse_token :: Eq token => token -> String -> Parser' token ()
  parse_token token = parse_token' (parse_certain_token token)
  -- | Parses tokens that fit a certain pattern and transforms them into something more useful - for example, a string or an
  -- integer. You also have to provide a string that briefly describes the kind of token that is expected - it is used to
  -- provide detailed parse errors.
  parse_token' :: Eq token => (token -> Maybe t) -> String -> Parser' token t
  parse_token' f expected =
    do
      tokens <- state_tokens <$> get_Parser
      case take_token f tokens of
        Nothing -> parse_error expected
        Just (x, tokens') ->
          do
            modify_Parser (\st -> st {state_tokens = tokens'})
            return x
  throwError_Parser :: Parse_error' token -> Parser' token t
  throwError_Parser err = Parser (throwError err)