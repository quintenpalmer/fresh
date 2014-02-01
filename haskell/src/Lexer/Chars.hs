module Lexer.Chars (
    is_eof,
    is_delimiter,
    is_whitespace,
    is_line_break,
    eof
) where

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
