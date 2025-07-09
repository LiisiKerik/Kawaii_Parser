{-|
Description: Assorted utilities.

* Assorted utilities.
* Relative directories and file paths.
-}
module Parser.Utilities (
  Back (..),
  Directory (..),
  File_path (..),
  (<->),
  (<//>),
  all_equal,
  between,
  check,
  check_ext,
  construct_map,
  construct_set,
  drop_file_name,
  element_at,
  lcm_all,
  read_file,
  swap_either,
  write_file_path) where
  import Control.Monad.Except
  import Data.Foldable as Foldable
  import Data.List as List
  import Data.Map as Map
  import Data.Set as Set
  import System.Directory
  -- | Parent directory.
  data Back = Back
  -- | Relative directory.
  data Directory = Directory [Back] [String]
  -- | Relative file path.
  data File_path = File_path Directory String String
  -- | Concatenate strings with a whitespace in between.
  infixr 5 <->
  (<->) :: String -> String -> String
  s <-> t = s <> " " <> t
  -- | Prepend a directory to file path.
  infixr 6 <//>
  (<//>) :: Directory -> File_path -> File_path
  directory_0 <//> File_path directory_1 file_name ext = File_path (directory_0 <> directory_1) file_name ext
  deriving instance Eq Back
  deriving instance Eq Directory
  deriving instance Eq File_path
  instance Monoid Directory where
    mempty = Directory [] []
  deriving instance Ord Back
  deriving instance Ord Directory
  deriving instance Ord File_path
  instance Semigroup Directory where
    Directory back_0 directories_0 <> Directory back_1 directories_1 =
      case (Foldable.length directories_0, Foldable.length back_1) of
        (0, 0) -> Directory back_0 directories_1
        (0, _) -> Directory (back_0 <> back_1) directories_1
        (_, 0) -> Directory back_0 (directories_0 <> directories_1)
        (_, _) -> Directory back_0 (init directories_0) <> Directory (List.drop 1 back_1) directories_1
  deriving instance Show Back
  deriving instance Show Directory
  deriving instance Show File_path
  -- | Checks if all elements of a list are equal. Returns @Nothing@ if all elements aren't equal, @Just Nothing@ if the list is
  -- empty and @Just (Just x)@ if all elements are equal to @x@.
  all_equal :: Eq t => [t] -> Maybe (Maybe t)
  all_equal x =
    case x of
      [] -> Just Nothing
      y : x' ->
        do
          check () (all ((==) y) x')
          Just (Just y)
  -- | Check if the value is in bounds.
  between :: Ord t => t -> t -> t -> Bool
  between lowest highest x = lowest <= x && highest >= x
  -- | Throw an error if the condition is not satisfied.
  check :: MonadError error f => error -> Bool -> f ()
  check err condition =
    case condition of
      False -> throwError err
      True -> return ()
  -- | Check file path extension.
  check_ext :: String -> File_path -> Bool
  check_ext ext (File_path _ _ ext') = ext == ext'
  -- | Construct a map if all keys are different. Otherwise return @Nothing@.
  construct_map :: Ord t => [(t, u)] -> Maybe (Map t u)
  construct_map x =
    do
      let y = Map.fromList x
      check () (Foldable.length x == Map.size y)
      Just y
  -- | Construct a set if all elements are different. Otherwise return @Nothing@.
  construct_set :: Ord t => [t] -> Maybe (Set t)
  construct_set x =
    do
      let y = Set.fromList x
      check () (Foldable.length x == Set.size y)
      Just y
  -- | Get the directory part of a file path.
  drop_file_name :: File_path -> Directory
  drop_file_name (File_path directory _ _) = directory
  -- | Safe list indexation.
  element_at :: Int -> [t] -> Maybe t
  element_at j x =
    do
      check () (between 0 (Foldable.length x - 1) j)
      return (x !! j)
  -- | Aggregate least common denominator.
  lcm_all :: (Foldable f, Integral t) => f t -> t
  lcm_all = Foldable.foldr lcm 1
  -- | Read a file.
  read_file :: File_path -> IO (Maybe String)
  read_file file_path =
    do
      let file_path' = write_file_path file_path
      pathExists <- doesPathExist file_path'
      case pathExists of
        False -> return Nothing
        True -> Just <$> readFile file_path'
  -- | Swap Left and Right.
  swap_either :: Either t u -> Either u t
  swap_either x =
    case x of
      Left y -> Right y
      Right y -> Left y
  write_back :: Back -> String
  write_back Back = ".."
  -- | Convert a file path to text.
  write_file_path :: File_path -> FilePath
  write_file_path (File_path (Directory back directories) file_name ext) =
    intercalate "/" ((write_back <$> back) <> directories <> [file_name <> "." <> ext])