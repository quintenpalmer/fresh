module Lexer.SourceInfo (
    TokenLoc(..),
    FileLoc(..),
    print_file_info
) where

import qualified Tools.Formatting as Formatting

data FileLoc = FileLoc Int Int deriving (Show)

data TokenLoc = TokenLoc FileLoc FileLoc deriving (Show)

print_file_info :: TokenLoc -> String
print_file_info (TokenLoc (FileLoc start_line start_char) (FileLoc end_line end_char)) =
    "line: " ++ Formatting.prefix_spaces (show start_line) 3 ++
    " char: " ++ Formatting.prefix_spaces (show start_char) 3 ++
    " to line: " ++ Formatting.prefix_spaces (show end_line) 3 ++
    " char: " ++ Formatting.prefix_spaces (show end_char) 3
