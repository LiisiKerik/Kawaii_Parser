{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE StandaloneDeriving #-}
{-|
Description: Locations of elements in a text file

* Locations of elements in a text file
* Writing locations
-}
module Parser.Line_and_char (L (..), Line_and_char, init_line_and_char, next_char, next_line, write_line_and_char) where
  -- | Add a location to any type.
  data L t = L Line_and_char t
  -- | The location of an element in a text file.
  data Line_and_char = Line_and_char Integer Integer
  deriving instance Eq Line_and_char
  deriving instance Ord Line_and_char
  deriving instance Show t => Show (L t)
  deriving instance Show Line_and_char
  -- | First line, first character.
  init_line_and_char :: Line_and_char
  init_line_and_char = Line_and_char 1 1
  -- | Move one character to the right.
  next_char :: Line_and_char -> Line_and_char
  next_char (Line_and_char line char) = Line_and_char line (1 + char)
  -- | Move to the beginning of the next line.
  next_line :: Line_and_char -> Line_and_char
  next_line (Line_and_char line _) = Line_and_char (1 + line) 1
  -- | Write the location with a colon between line and character number.
  write_line_and_char :: Line_and_char -> String
  write_line_and_char (Line_and_char line char) = show line ++ ":" ++ show char