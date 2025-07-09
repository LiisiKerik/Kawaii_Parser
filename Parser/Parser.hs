{-|
Description: Tokenising and parsing.

* A combinator-based tokeniser with symmetric choice and location tracking.
* An unambiguous parser with symmetric choice and location tracking.
-}
module Parser.Parser (
  Parse_error (..),
  Parser',
  Tokeniser',
  Usage_error (..),
  (<+>),
  add_location,
  add_token,
  filter_parser,
  fmap_filter_parser,
  parse',
  parse_brackets,
  parse_default,
  parse_file_path',
  parse_list,
  parse_location,
  parse_many,
  parse_non_empty_list,
  parse_not,
  parse_some,
  parse_token,
  parse_token',
  parse_with_location) where
  import Control.Lens.Combinators
  import Control.Lens.Operators
  import Control.Monad.Except
  import Control.Monad.RWS.Strict
  import Data.Generics.Labels ()
  import GHC.Generics
  import Parser.Locations
  import Parser.Utilities
  newtype Parser token output error t =
    Parser {run_parser' :: RWST () output (Parser.Parser.State token) (ExceptT (Parse_error error) (Either Usage_error)) t}
  -- | A parser that works on any kind of tokens.
  type Parser' token error = Parser token () error
  -- | Parse errors.
  data Parse_error error = Filter_error error | Parse_error' Location
  data State token = State {state_tokens :: Tokens token, state_lookahead :: Location}
  -- | A tokeniser that works on any kind of custom characters and tokens. The custom character type is useful if you need to
  -- classify characters before tokenisation to simplify patternmatching.
  type Tokeniser' char_class token error = Parser char_class [With_location token] error
  -- | A sequence of tokens with locations. For internal use in the parser.
  data Tokens token = Tokens [With_location token] Location
  -- | Errors that indicate that the parsing library has been used incorrectly.
  data Usage_error = Ambiguity | Attempt_to_recover_a_filter_error | Filter_error_encountered_in_parse_not
  -- | Symmetric choice between two parsers that selects the longest match. Note that if both parsers successfully reach the
  -- same location it will result in an ambiguity error. Also note that you should not attempt to recover a filter error. This
  -- operator is normally associative unless you make a mistake and write an ambiguous parser.
  infixr 3 <+>
  (<+>) :: Parser token output error t -> Parser token output error t -> Parser token output error t
  Parser (RWST parse_0) <+> Parser (RWST parse_1) =
    Parser
      (RWST
        (\ () st ->
          ExceptT
            (do
              (lookahead_0, result_0) <- deconstruct_result (parse_0 () st)
              (lookahead_1, result_1) <- deconstruct_result (parse_1 () st)
              (
                construct_result (max lookahead_0 lookahead_1) <$>
                case (result_0, result_1) of
                  (Nothing, Nothing) -> Right Nothing
                  (Nothing, Just _) -> Right result_1
                  (Just _, Nothing) -> Right result_0
                  (Just (_, tokens_0, _), Just (_, tokens_1, _)) ->
                    case compare (current_location tokens_0) (current_location tokens_1) of
                      LT -> Right result_1
                      EQ -> Left Ambiguity
                      GT -> Right result_0))))
  instance Monoid output => Applicative (Parser token output error) where
    Parser parse_0 <*> Parser parse_1 = Parser (parse_0 <*> parse_1)
    pure x = Parser (return x)
  instance Functor (Parser token output error) where
    fmap f (Parser parse_t) = Parser (f <$> parse_t)
  deriving instance Generic (Parser.Parser.State token)
  instance Monoid output => Monad (Parser token output error) where
    Parser parse_t >>= f = Parser (parse_t >>= run_parser' <$> f)
  deriving instance Show error => Show (Parse_error error)
  deriving instance Show token => Show (Parser.Parser.State token)
  deriving instance Show token => Show (Tokens token)
  deriving instance Show Usage_error
  -- | Parse with location.
  add_location :: Monoid output => Parser token output error t -> Parser token output error (With_location t)
  add_location parse_t = With_location <$> parse_location <*> parse_t
  -- | Add the token to the output.
  add_token :: Tokeniser' char_class token error token -> Tokeniser' char_class token error ()
  add_token tokenise =
    do
      location <- parse_location
      token <- tokenise
      Parser (tell [With_location location token])
  certain_token :: Eq token => token -> token -> Maybe ()
  certain_token token token' =
    do
      check () (token == token')
      return ()
  construct_result ::
    Location -> Maybe (t, Tokens token, output) -> Either (Parse_error error) (t, Parser.Parser.State token, output)
  construct_result lookahead result =
    case result of
      Nothing -> Left (Parse_error' lookahead)
      Just (x, tokens, output) -> Right (x, State tokens lookahead, output)
  -- | Get the location of first token or, if there are none, the end of file. For internal use in the parser.
  current_location :: Tokens token -> Location
  current_location (Tokens tokens end_location) =
    case tokens of
      [] -> end_location
      With_location location _ : _ -> location
  deconstruct_result ::
    (
      ExceptT (Parse_error error) (Either Usage_error) (t, Parser.Parser.State token, output) ->
      Either Usage_error (Location, Maybe (t, Tokens token, output)))
  deconstruct_result (ExceptT maybe_result) =
    do
      result <- maybe_result
      case result of
        Left err ->
          case err of
            Filter_error _ -> Left Attempt_to_recover_a_filter_error
            Parse_error' lookahead -> Right (lookahead, Nothing)
        Right (x, State tokens lookahead, output) -> Right (lookahead, Just (x, tokens, output))
  -- | Filter the parse results - for example, restrict an integer parser to positive numbers. You also have to provide an
  -- error. Note that filter errors, unlike token matching errors, are non-recoverable.
  filter_parser :: (Eq token, Monoid output) =>
    (t -> Bool) -> (Location -> error) -> Parser token output error t -> Parser token output error t
  filter_parser f err =
    fmap_filter_parser
      (\ x ->
        do
          check err (f x)
          Right x)
  -- | Filter and transform the parse results in one operation. You also have to provide an error. Note that filter errors,
  -- unlike matching errors, are non-recoverable.
  fmap_filter_parser :: (Eq token, Monoid output) =>
    (t -> Either (Location -> error) u) -> Parser token output error t -> Parser token output error u
  fmap_filter_parser f parse_t =
    do
      location <- parse_location
      x <- parse_t
      case f x of
        Left err -> Parser (throwError (Filter_error (err location)))
        Right y -> return y
  get_tokens :: Monoid output => Parser token output error (Tokens token)
  get_tokens =
    do
      tokens <- Parser (use #state_tokens)
      Parser (#state_lookahead %= max (current_location tokens))
      return tokens
  -- | Parse the text. You have to provide a function that classifies characters, a function that updates the location after
  -- each character, a tokeniser, a parser and a function that converts parse errors to your preferred type.
  parse' :: forall char_class token error t .
    (
      (Char -> char_class) ->
      (char_class -> Location -> Location) ->
      Tokeniser' char_class token error () ->
      Parser' token error t ->
      (Location -> error) ->
      String ->
      Either Usage_error (Either error t))
  parse' classify_char next_location tokenise_t parse_t parse_error text =
    runExceptT
      (withExceptT
        transform_error
        (do
          let (end_location, text') = execRWS (classify_chars text) () init_location
          ((), tokens) <- run_parser tokenise_t (Tokens text' end_location)
          fst <$> run_parser parse_t (Tokens tokens end_location)))
    where
      classify_chars :: String -> RWS () [With_location char_class] Location ()
      classify_chars text' =
        case text' of
          [] -> return ()
          c : text'' ->
            do
              location <- get
              let char_class = classify_char c
              tell [With_location location char_class]
              modify (next_location char_class)
              classify_chars text''
      transform_error :: Parse_error error -> error
      transform_error err =
        case err of
          Filter_error err' -> err'
          Parse_error' location -> parse_error location
  -- | Parse a term in brackets.
  parse_brackets :: (Eq token, Monoid output) => token -> token -> Parser token output error t -> Parser token output error t
  parse_brackets left_bracket right_bracket parse_t =
    do
      parse_token left_bracket
      x <- parse_t
      parse_token right_bracket
      return x
  -- | Parse something optional or return the default value.
  parse_default :: Monoid output => Parser token output error t -> t -> Parser token output error t
  parse_default parse_t x = return x <+> parse_t
  parse_directory ::
    Eq token => Parser' token error [Back] -> Parser' token error String -> token -> Parser' token error Directory
  parse_directory parse_back parse_name slash_token =
    Directory <$> parse_back <*> parse_many (parse_one_directory parse_name slash_token)
  parse_element :: (Eq token, Monoid output) => token -> Parser token output error t -> Parser token output error t
  parse_element separator parse_t =
    do
      parse_token separator
      parse_t
  parse_end :: Monoid output => Parser token output error t -> Parser token output error t
  parse_end parse_t =
    do
      x <- parse_t
      parse_end'
      return x
  parse_end' :: Monoid output => Parser token output error ()
  parse_end' =
    do
      tokens <- get_tokens
      case tokens_ended tokens of
        False -> parse_error'
        True -> return ()
  parse_error' :: Monoid output => Parser token output error t
  parse_error' =
    do
      lookahead <- Parser (use #state_lookahead)
      Parser (throwError (Parse_error' lookahead))
  -- | Parse a file path. You have to provide a parent directory counter parser, a name parser, the slash token, the dot token,
  -- a function that converts strings to name tokens and the file extension.
  parse_file_path' :: Eq token =>
    (
      Parser' token error [Back] ->
      Parser' token error String ->
      token ->
      token ->
      (String -> token) ->
      String ->
      Parser' token error File_path)
  parse_file_path' parse_back parse_name slash_token dot_token name_token ext =
    do
      directory <- parse_directory parse_back parse_name slash_token
      file_name <- parse_name
      parse_token dot_token
      parse_token (name_token ext)
      return (File_path directory file_name ext)
  -- | Parse a (possibly empty) list with separators.
  parse_list :: (Eq token, Monoid output) => token -> Parser token output error t -> Parser token output error [t]
  parse_list separator parse_t = parse_default (parse_non_empty_list separator parse_t) []
  -- | Get the current location.
  parse_location :: Monoid output => Parser token output error Location
  parse_location = current_location <$> Parser (use #state_tokens)
  -- | Parse a (possibly empty) list without separators.
  parse_many :: Monoid output => Parser token output error t -> Parser token output error [t]
  parse_many parse_t = parse_default (parse_some parse_t) []
  -- | Parse a non-empty list with separators.
  parse_non_empty_list :: (Eq token, Monoid output) => token -> Parser token output error t -> Parser token output error [t]
  parse_non_empty_list separator parse_t = (:) <$> parse_t <*> parse_many (parse_element separator parse_t)
  -- | Succeed if the parser fails.
  parse_not :: Monoid output => Parser token output error t -> Parser token output error ()
  parse_not (Parser (RWST parse_t)) =
    Parser
      (RWST
        (\ () st ->
          ExceptT
            (do
              result <- runExceptT (parse_t () st)
              case result of
                Left err ->
                  case err of
                    Filter_error _ -> Left Filter_error_encountered_in_parse_not
                    Parse_error' location -> Right (Right ((), st {state_lookahead = location}, mempty))
                Right (_, State _ lookahead', _) -> Right (Left (Parse_error' lookahead')))))
  parse_one_directory :: Eq token => Parser' token error t -> token -> Parser' token error t
  parse_one_directory parse_name slash_token =
    do
      directory <- parse_name
      parse_token slash_token
      return directory
  -- | Parse a non-empty list without separators.
  parse_some :: Monoid output => Parser token output error t -> Parser token output error [t]
  parse_some parse_t = (:) <$> parse_t <*> parse_many parse_t
  -- | Parse a certain token (for example, a delimiter or a keyword) without returning a result.
  parse_token :: (Eq token, Monoid output) => token -> Parser token output error ()
  parse_token token = parse_token' (certain_token token)
  -- | Parse tokens that fit a certain pattern and transform them into something more useful - for example, an int or a string.
  parse_token' :: Monoid output => (token -> Maybe t) -> Parser token output error t
  parse_token' f =
    do
      tokens <- get_tokens
      case take_token f tokens of
        Nothing -> parse_error'
        Just (x, tokens') ->
          do
            Parser (#state_tokens .= tokens')
            return x
  -- | Parse data with location.
  parse_with_location :: Monoid output => Parser token output error t -> Parser token output error (With_location t)
  parse_with_location parse_t = With_location <$> parse_location <*> parse_t
  run_parser :: Monoid output =>
    Parser token output error t -> Tokens token -> ExceptT (Parse_error error) (Either Usage_error) (t, output)
  run_parser parse_t tokens = evalRWST (run_parser' (parse_end parse_t)) () (Parser.Parser.State tokens init_location)
  take_token :: (token -> Maybe t) -> Tokens token -> Maybe (t, Tokens token)
  take_token f (Tokens tokens end_location) =
    case tokens of
      [] -> Nothing
      With_location _ token : tokens' -> flip (,) (Tokens tokens' end_location) <$> f token
  tokens_ended :: Tokens token -> Bool
  tokens_ended (Tokens tokens _) = null tokens