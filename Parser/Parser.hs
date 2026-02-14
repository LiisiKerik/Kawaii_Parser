{-|
Description: Combinator-based tokeniser and parser with symmetric choice and location tracking.

* Combinator-based tokeniser and parser with symmetric choice and location tracking.
-}
module Parser.Parser (
  Parser',
  Tokeniser',
  (<+>),
  add_location,
  add_token,
  filter_parser,
  fmap_filter_parser,
  parse',
  parse_brackets,
  parse_default,
  parse_list,
  parse_location,
  parse_many,
  parse_non_empty_list,
  parse_some,
  parse_token,
  parse_token') where
  import Control.Monad.Except
  import Control.Monad.RWS.Strict
  import Control.Monad.State.Strict
  import Control.Monad.Trans.Maybe
  import Control.Monad.Writer.Strict
  import Data.Foldable
  import Parser.Locations
  import Parser.Utilities
  newtype Parser token output error t =
    Parser {run_parser' :: Tokens token -> (Location, Maybe (Maybe (Either error (t, output)), Tokens token))}
  -- | A parser that works on any kind of tokens.
  type Parser' token error = Parser token () error
  -- | A tokeniser that works on any kind of custom characters and tokens. The custom character type is useful if you need to
  -- classify characters before tokenisation to simplify patternmatching.
  type Tokeniser' char_class token error = Parser char_class [With_location token] error
  data Tokens token = Tokens [With_location token] Location
  -- | Symmetric choice between two parsers.
  infixr 3 <+>
  (<+>) :: (Monoid output) => Parser token output error t -> Parser token output error t -> Parser token output error t
  Parser parse_0 <+> Parser parse_1 =
    Parser
      (\ tokens ->
        do
          result_0 <- parse_0 tokens
          result_1 <- parse_1 tokens
          return
            (case (result_0, result_1) of
              (Nothing, _) -> result_1
              (_, Nothing) -> result_0
              (Just (_, tokens_0), Just (_, tokens_1)) ->
                case compare (count_tokens tokens_0) (count_tokens tokens_1) of
                  LT -> result_0
                  EQ -> Just (Nothing, tokens_0)
                  GT -> result_1))
  instance (Monoid output) => Applicative (Parser token output error) where
    parse_0 <*> parse_1 = monad_to_parser (parser_to_monad parse_0 <*> parser_to_monad parse_1)
    pure x = monad_to_parser (return x)
  instance Functor (Parser token output error) where
    fmap f parse = monad_to_parser (f <$> parser_to_monad parse)
  instance (Monoid output) => Monad (Parser token output error) where
    parse >>= f = monad_to_parser (parser_to_monad parse >>= parser_to_monad <$> f)
  deriving instance (Show token) => Show (Tokens token)
  -- | Parse with location.
  add_location :: (Monoid output) => Parser token output error t -> Parser token output error (With_location t)
  add_location parse_t = With_location <$> parse_location <*> parse_t
  -- | Add the token to the output.
  add_token :: Tokeniser' char_class token error token -> Tokeniser' char_class token error ()
  add_token tokenise =
    do
      location <- parse_location
      token <- tokenise
      monad_to_parser (tell [With_location location token])
  certain_token :: (Eq token) => token -> token -> Maybe ()
  certain_token token token' =
    do
      check () (token == token')
      return ()
  count_tokens :: Tokens token -> Int
  count_tokens (Tokens tokens _) = length tokens
  -- | Get the location of first token or, if there are none, the end of file.
  current_location :: Tokens token -> Location
  current_location (Tokens tokens end_location) =
    case tokens of
      [] -> end_location
      With_location location _ : _ -> location
  filter_error :: error -> Parser token output error t
  filter_error err = Parser (\ tokens -> return (Just (Just (Left err), tokens)))
  -- | Filter parse results - for example, restrict an integer parser to positive numbers. You also have to provide an error.
  filter_parser :: (Eq token, Monoid output) =>
    (t -> Bool) -> (Location -> error) -> Parser token output error t -> Parser token output error t
  filter_parser f err =
    fmap_filter_parser
      (\ x ->
        do
          check err (f x)
          Right x)
  -- | Filter and transform the parse results in one operation. You also have to provide an error.
  fmap_filter_parser :: (Eq token, Monoid output) =>
    (t -> Either (Location -> error) u) -> Parser token output error t -> Parser token output error u
  fmap_filter_parser f parse_t =
    do
      location <- parse_location
      x <- parse_t
      case f x of
        Left err -> filter_error (err location)
        Right y -> return y
  get_tokens :: (Monoid output) => Parser token output error (Tokens token)
  get_tokens =
    do
      tokens <- monad_to_parser get
      update_lookahead (current_location tokens)
      return tokens
  monad_to_parser ::
    (
      WriterT output (ExceptT error (MaybeT (StateT (Tokens token) (MaybeT ((,) Location))))) t ->
      Parser token output error t)
  monad_to_parser (WriterT (ExceptT (MaybeT (StateT parse)))) = Parser (\ tokens -> runMaybeT (parse tokens))
  -- | Parse the text. You have to provide a function that classifies characters, a function that updates the location after
  -- each character, a tokeniser, a parser and a function that converts parse errors to your preferred type. Note that if there
  -- is more than one way to successfully parse the text you will get @Nothing@ as the result.
  parse' :: forall char_class token error t .
    (
      (Char -> char_class) ->
      (char_class -> Location -> Location) ->
      Tokeniser' char_class token error () ->
      Parser' token error t ->
      (Location -> error) ->
      String ->
      Maybe (Either error t))
  parse' classify_char next_location tokenise_t parse_t parse_error' text =
    runExceptT
      (do
        let (end_location, text') = execRWS (traverse_ classify_char' text) () init_location
        ((), tokens) <- run_parser tokenise_t (Tokens text' end_location)
        fst <$> run_parser parse_t (Tokens tokens end_location))
    where
      classify_char' :: Char -> RWS () [With_location char_class] Location ()
      classify_char' c =
        do
          location <- get
          let char_class = classify_char c
          tell [With_location location char_class]
          modify (next_location char_class)
      run_parser :: (Monoid output) => Parser token' output error u -> Tokens token' -> ExceptT error Maybe (u, output)
      run_parser parse_u tokens =
        let
          (lookahead, maybe_result) = run_parser' (parse_end parse_u) tokens in
          case maybe_result of
            Nothing -> throwError (parse_error' lookahead)
            Just (result, _) -> ExceptT result
  -- | Parse a term in brackets.
  parse_brackets :: (Eq token, Monoid output) => token -> token -> Parser token output error t -> Parser token output error t
  parse_brackets left_bracket right_bracket parse_t =
    do
      parse_token left_bracket
      x <- parse_t
      parse_token right_bracket
      return x
  -- | Parse something optional or return the default value.
  parse_default :: (Monoid output) => t -> Parser token output error t -> Parser token output error t
  parse_default x parse_t = return x <+> parse_t
  parse_element :: (Eq token, Monoid output) => token -> Parser token output error t -> Parser token output error t
  parse_element separator parse_t =
    do
      parse_token separator
      parse_t
  parse_end :: (Monoid output) => Parser token output error t -> Parser token output error t
  parse_end parse_t =
    do
      x <- parse_t
      parse_end'
      return x
  parse_end' :: (Monoid output) => Parser token output error ()
  parse_end' =
    do
      tokens <- get_tokens
      case tokens_ended tokens of
        False -> parse_error
        True -> return ()
  parse_error :: (Monoid output) => Parser token output error t
  parse_error = Parser (\ _ -> return Nothing)
  -- | Parse a (possibly empty) list with separators.
  parse_list :: (Eq token, Monoid output) => token -> Parser token output error t -> Parser token output error [t]
  parse_list separator parse_t = parse_default [] (parse_non_empty_list separator parse_t)
  -- | Get the current location.
  parse_location :: (Monoid output) => Parser token output error Location
  parse_location = current_location <$> monad_to_parser get
  -- | Parse a (possibly empty) list without separators.
  parse_many :: (Monoid output) => Parser token output error t -> Parser token output error [t]
  parse_many parse_t = parse_default [] (parse_some parse_t)
  -- | Parse a non-empty list with separators.
  parse_non_empty_list :: (Eq token, Monoid output) => token -> Parser token output error t -> Parser token output error [t]
  parse_non_empty_list separator parse_t = (:) <$> parse_t <*> parse_many (parse_element separator parse_t)
  -- | Parse a non-empty list without separators.
  parse_some :: (Monoid output) => Parser token output error t -> Parser token output error [t]
  parse_some parse_t = (:) <$> parse_t <*> parse_many parse_t
  -- | Parse a certain token (for example, a delimiter or a keyword) without returning a result.
  parse_token :: (Eq token, Monoid output) => token -> Parser token output error ()
  parse_token token = parse_token' (certain_token token)
  -- | Parse tokens that fit a certain pattern and transform them into something more useful - for example, an int or a string.
  parse_token' :: (Monoid output) => (token -> Maybe t) -> Parser token output error t
  parse_token' f =
    do
      tokens <- get_tokens
      case take_token f tokens of
        Nothing -> parse_error
        Just (x, tokens') ->
          do
            monad_to_parser (put tokens')
            return x
  parser_to_monad ::
    (
      Parser token output error t ->
      WriterT output (ExceptT error (MaybeT (StateT (Tokens token) (MaybeT ((,) Location))))) t)
  parser_to_monad (Parser parse) = WriterT (ExceptT (MaybeT (StateT (\ tokens -> MaybeT (parse tokens)))))
  take_token :: (token -> Maybe t) -> Tokens token -> Maybe (t, Tokens token)
  take_token f (Tokens tokens end_location) =
    case tokens of
      [] -> Nothing
      With_location _ token : tokens' -> flip (,) (Tokens tokens' end_location) <$> f token
  tokens_ended :: Tokens token -> Bool
  tokens_ended (Tokens tokens _) = null tokens
  update_lookahead :: (Monoid output) => Location -> Parser token output error ()
  update_lookahead lookahead =
    Parser
      (\ tokens ->
        do
          tell lookahead
          return (Just (Just (Right ((), mempty)), tokens)))