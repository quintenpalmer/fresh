module Lexer.SourceInfo (
    TokenLoc(..),
    FileLoc(..),
    print_file_info
) where

data FileLoc = FileLoc Int Int deriving (Show)

data TokenLoc = TokenLoc FileLoc FileLoc deriving (Show)

print_file_info :: TokenLoc -> String
print_file_info (TokenLoc (FileLoc start_line start_char) (FileLoc end_line end_char)) =
    "starting at line " ++ (show start_line) ++
    " char " ++ (show start_char) ++
    ". ending at line " ++ (show end_line) ++
    " char " ++ (show end_char)
