module Lexer.SourceInfo (
    TokenLoc(..),
    FileLoc(..)
) where

import qualified Tools.FreshShow as FreshShow
import qualified Tools.Formatting as Formatting

data FileLoc = FileLoc Int Int deriving (Show)

data TokenLoc = TokenLoc FileLoc FileLoc deriving (Show)

instance FreshShow.FullShow TokenLoc where
    full_show (TokenLoc (FileLoc start_line start_char) (FileLoc end_line end_char)) =
        "line: " ++ Formatting.prefix_spaces (show start_line) 3 ++
        " char: " ++ Formatting.prefix_spaces (show start_char) 3 ++
        " to line: " ++ Formatting.prefix_spaces (show end_line) 3 ++
        " char: " ++ Formatting.prefix_spaces (show end_char) 3

instance FreshShow.ShortShow TokenLoc where
    short_show (TokenLoc (FileLoc start_line start_char) (FileLoc end_line end_char)) =
        "(" ++ (show start_line) ++
        ":" ++ (show start_char) ++
        ") - (" ++ (show end_line) ++
        ":" ++ (show end_char) ++
        ")"
