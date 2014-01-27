module Lexer.StringParse (
    get_next_non_whitespace,
    get_next_character,
    is_int_literal,
    is_bool_literal,
    eof,
    whitespace,
    delimiters,
    FileLocInfo(..),
    FileLoc(..),
    print_file_info
) where

import qualified Data.Char as Char

data FileLoc = FileLoc Int Int deriving (Show)

data FileLocInfo = FileLocInfo FileLoc FileLoc deriving (Show)

print_file_info :: FileLocInfo -> String
print_file_info (FileLocInfo (FileLoc start_char start_line) (FileLoc end_char end_line)) =
    "starting at char " ++ (show start_char) ++ " line " ++ (show start_line) ++ ". ending at char " ++ (show end_char) ++ " line " ++ (show end_line)

bools :: [String]
bools = ["true", "false"]

is_bool_literal :: String -> Bool
is_bool_literal string =
    string `elem` bools

is_int_literal :: String -> Bool
is_int_literal ('-':string) =
    are_digits string
is_int_literal string =
    are_digits string

are_digits :: String -> Bool
are_digits [] = False
are_digits string =
    all Char.isDigit string

eof :: Char
eof = '\0'

line_breaks :: [Char]
line_breaks = ['\n']


inline_whitespace :: [Char]
inline_whitespace = [' ', '\r', '\t']

whitespace :: [Char]
whitespace = inline_whitespace ++ line_breaks

delimiters :: [Char]
delimiters = ['(', ')', '[', ']', eof] ++ whitespace

get_next_non_whitespace :: String -> FileLocInfo -> (Char, String, FileLocInfo)
get_next_non_whitespace remaining input_info =
    let (current, post_remaining, info) = get_next_character remaining input_info
    in
        if current `elem` whitespace then
            get_next_non_whitespace post_remaining info
        else
            (current, post_remaining, info)

get_next_character :: String -> FileLocInfo -> (Char, String, FileLocInfo)
get_next_character [] i = (eof, [eof], i)
get_next_character [current] info = (current, [eof], info)
get_next_character (current: rest) (FileLocInfo (FileLoc start_char start_line) (FileLoc end_char end_line)) =
    if current `elem` line_breaks then
        (current, rest, (FileLocInfo (FileLoc start_char (start_line + 1)) (FileLoc end_char (end_line + 1))))
    else
        (current, rest, (FileLocInfo (FileLoc (start_char + 1) start_line) (FileLoc (end_char + 1) end_line)))
