{-|
Description: Relative directories and file paths.

* Relative directories and file paths.
-}
module Parser.Files (
  Back (..),
  Directory (..),
  Error (..),
  Ext,
  File_path (..),
  File_path_and_location (..),
  (<//>),
  drop_file_name,
  parse_file_path,
  read_file,
  write_file,
  write_file_path_and_location) where
  import Control.Monad
  import Control.Monad.Except
  import Control.Monad.IO.Class
  import Data.Char
  import Data.Maybe
  import Parser.Locations
  import Parser.Parser
  import Parser.Utilities
  import System.Directory
  -- | Parent directory.
  data Back = Back
  data Char_class = Delimiter_char Token | Invalid_char | Name_char Char
  -- | Relative directory.
  data Directory = Directory [Back] [String]
  -- | Errors.
  data Error = Failed_to_find_the_file FilePath | Invalid_file_path | Unexpected_file_extension Ext
  type Ext = String
  -- | Relative file path.
  data File_path = File_path Directory String Ext
  -- | Locations with a file path.
  data File_path_and_location = File_path_and_location File_path Location
  type Parser = Parser' Token Error
  data Token = Dot_token | Name_token String | Slash_token
  type Tokeniser = Tokeniser' Char_class Token Error
  -- | Prepend a directory to file path.
  infixr 6 <//>
  (<//>) :: Directory -> File_path -> File_path
  directory_0 <//> File_path directory_1 file_name ext = File_path (directory_0 <> directory_1) file_name ext
  deriving instance Eq Back
  deriving instance Eq Char_class
  deriving instance Eq Directory
  deriving instance Eq File_path
  deriving instance Eq Token
  instance Monoid Directory where
    mempty = Directory [] []
  deriving instance Ord Back
  deriving instance Ord Directory
  deriving instance Ord File_path
  instance Semigroup Directory where
    Directory back_0 directories_0 <> Directory back_1 directories_1 =
      case (directories_0, back_1) of
        ([], _) -> Directory (back_0 <> back_1) directories_1
        (_, []) -> Directory back_0 (directories_0 <> directories_1)
        (_ : _, Back : _) -> Directory back_0 (init directories_0) <> Directory (tail back_1) directories_1
  deriving instance Show Back
  deriving instance Show Char_class
  deriving instance Show Directory
  deriving instance Show Error
  deriving instance Show File_path
  deriving instance Show File_path_and_location
  deriving instance Show Token
  check_ext :: (MonadError Error f) => Ext -> File_path -> f ()
  check_ext expected_ext (File_path _ _ actual_ext) = check (Unexpected_file_extension actual_ext) (expected_ext == actual_ext)
  check_file_path :: (MonadError Error f) => Ext -> File_path -> f ()
  check_file_path ext file_path =
    do
      check Invalid_file_path (valid_file_path file_path)
      check_ext ext file_path
  classify_char :: Char -> Char_class
  classify_char c =
    case c of
      _ | valid_name_char c -> Name_char c
      '.' -> Delimiter_char Dot_token
      '/' -> Delimiter_char Slash_token
      _ -> Invalid_char
  delimiter_char :: Char_class -> Maybe Token
  delimiter_char char_class =
    case char_class of
      Delimiter_char token -> Just token
      _ -> Nothing
  -- | Get the directory part of a file path.
  drop_file_name :: File_path -> Directory
  drop_file_name (File_path directory _ _) = directory
  name_char :: Char_class -> Maybe Char
  name_char char_class =
    case char_class of
      Name_char c -> Just c
      _ -> Nothing
  name_token :: Token -> Maybe String
  name_token token =
    case token of
      Name_token name -> Just name
      _ -> Nothing
  parse_back :: Parser Back
  parse_back =
    do
      replicateM_ 2 parse_dot
      return Back
  parse_directory :: Parser Directory
  parse_directory = Directory <$> parse_many (parse_with_slash parse_back) <*> parse_many (parse_with_slash parse_name)
  parse_dot :: Parser ()
  parse_dot = parse_token Dot_token
  -- | Parse a file path and check the extension.
  parse_file_path :: Ext -> String -> Either Error File_path
  parse_file_path expected_ext file_path =
    do
      file_path' <- fromJust (parse' classify_char (\ _ -> id) tokenise parse_file_path' (\ _ -> Invalid_file_path) file_path)
      check_ext expected_ext file_path'
      return file_path' where
    parse_file_path' :: Parser File_path
    parse_file_path' =
      do
        directory <- parse_directory
        file_name <- parse_name
        parse_dot
        actual_ext <- parse_name
        return (File_path directory file_name actual_ext)
  parse_name :: Parser String
  parse_name = parse_token' name_token
  parse_with_slash :: Parser t -> Parser t
  parse_with_slash parse_t =
    do
      x <- parse_t
      parse_token Slash_token
      return x
  -- | Check the extension and read the file.
  read_file :: Ext -> File_path -> ExceptT Error IO String
  read_file ext file_path =
    do
      check_file_path ext file_path
      let file_path' = write_file_path file_path
      file_exits <- liftIO (doesPathExist file_path')
      case file_exits of
        False -> throwError (Failed_to_find_the_file file_path')
        True -> liftIO (readFile file_path')
  tokenise :: Tokeniser ()
  tokenise = void (parse_many tokenise_1)
  tokenise_1 :: Tokeniser ()
  tokenise_1 = tokenise_delimiter <+> tokenise_name
  tokenise_delimiter :: Tokeniser ()
  tokenise_delimiter = add_token (parse_token' delimiter_char)
  tokenise_name :: Tokeniser ()
  tokenise_name = add_token (Name_token <$> parse_some (parse_token' name_char))
  valid_directory :: Directory -> Bool
  valid_directory (Directory _ directories) = all valid_name directories
  valid_file_path :: File_path -> Bool
  valid_file_path (File_path directory file_name ext) =
    valid_directory directory && valid_name file_name && valid_name ext
  valid_name :: String -> Bool
  valid_name = all valid_name_char
  valid_name_char :: Char -> Bool
  valid_name_char c = elem c "'_" || isLetter c || isDigit c
  write_back :: Back -> String
  write_back Back = ".."
  write_directory :: Directory -> FilePath
  write_directory (Directory back directories) = join (write_with_slash <$> ((write_back <$> back) <> directories))
  -- | Check the extension and write to the file.
  write_file :: Ext -> File_path -> String -> ExceptT Error IO ()
  write_file ext file_path file =
    do
      check_file_path ext file_path
      liftIO (writeFile (write_file_path file_path) file)
  write_file_path :: File_path -> FilePath
  write_file_path (File_path directory file_name ext) = write_directory directory <> file_name <> "." <> ext
  write_file_path_and_location :: File_path -> Location -> String
  write_file_path_and_location file_path location = write_file_path file_path <> ":" <> write_location location
  write_with_slash :: String -> String
  write_with_slash text = text <> "/"