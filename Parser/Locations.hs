{-|
Description: Locations.

* Locations.
-}
module Parser.Locations (
  File_path_and_location (..),
  Location,
  With_location (..),
  init_location,
  next_char,
  next_line,
  write_file_path_and_location,
  write_location) where
  import Parser.Utilities
  deriving instance Eq Location
  deriving instance Ord Location
  deriving instance Show File_path_and_location
  deriving instance Show Location
  deriving instance Show t => Show (With_location t)
  -- | Locations with a file path.
  data File_path_and_location = File_path_and_location File_path Location
  -- | Locations.
  data Location = Location Integer Integer
  -- | Add a location to any type.
  data With_location t = With_location Location t
  -- | First line, first character.
  init_location :: Location
  init_location = Location 1 1
  -- | Move one character to the right.
  next_char :: Location -> Location
  next_char (Location line char) = Location line (1 + char)
  -- | Move to the next line.
  next_line :: Location -> Location
  next_line (Location line _) = Location (1 + line) 1
  -- | Write the location with the file path.
  write_file_path_and_location :: File_path -> Location -> String
  write_file_path_and_location file_path location = write_file_path file_path <> ":" <> write_location location
  -- | Write the location.
  write_location :: Location -> String
  write_location (Location line char) = show line <> ":" <> show char