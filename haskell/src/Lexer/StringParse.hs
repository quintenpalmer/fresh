module Lexer.StringParse (
    get_next_non_whitespace,
    get_next_character,
    is_delimiter,
    is_eof,
) where

import qualified Lexer.FileLoc as FileLoc

type FileLocInfo = FileLoc.FileLocInfo

eof :: Char
eof = '\0'

line_breaks :: [Char]
line_breaks = ['\n']

whitespace :: [Char]
whitespace = [' ', '\r', '\t'] ++ line_breaks

delimiters :: [Char]
delimiters = ['(', ')', '[', ']', eof] ++ whitespace

is_line_break :: Char -> Bool
is_line_break char =
    char `elem` line_breaks

is_whitespace :: Char -> Bool
is_whitespace char =
    char `elem` whitespace

is_delimiter :: Char -> Bool
is_delimiter char =
    char `elem` delimiters

is_eof :: String -> Bool
is_eof string =
    case string of
        [] -> False
        (head_:_) -> head_ == eof

get_next_non_whitespace :: String -> FileLocInfo -> (Char, String, FileLocInfo)
get_next_non_whitespace remaining input_info =
    let (current, post_remaining, info) = get_next_character remaining input_info
    in
        if is_whitespace current then
            get_next_non_whitespace post_remaining info
        else
            (current, post_remaining, info)

get_next_character :: String -> FileLocInfo -> (Char, String, FileLocInfo)
get_next_character [] _ = error "Reading past end of file"
get_next_character [current] info = (current, [eof], info)
get_next_character (current: rest) (FileLoc.FileLocInfo (FileLoc.FileLoc start_char start_line) (FileLoc.FileLoc end_char end_line)) =
    if is_line_break current then
        (current, rest, (FileLoc.FileLocInfo (FileLoc.FileLoc 1 start_line) (FileLoc.FileLoc 1 (end_line + 1))))
    else
        (current, rest, (FileLoc.FileLocInfo (FileLoc.FileLoc start_char start_line) (FileLoc.FileLoc (end_char + 1) end_line)))
