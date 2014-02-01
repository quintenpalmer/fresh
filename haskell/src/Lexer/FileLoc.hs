module Lexer.FileLoc (
    FileLocInfo(..),
    FileLoc(..),
    print_file_info
) where

data FileLoc = FileLoc Int Int deriving (Show)

data FileLocInfo = FileLocInfo FileLoc FileLoc deriving (Show)

print_file_info :: FileLocInfo -> String
print_file_info (FileLocInfo (FileLoc start_char start_line) (FileLoc end_char end_line)) =
    "starting at char " ++ (show start_char) ++
    " line " ++ (show start_line) ++
    ". ending at char " ++ (show end_char) ++
    " line " ++ (show end_line)
