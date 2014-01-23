module Lexer.StringParse (
    get_next_non_whitespace,
    get_next_character,
    is_int_literal,
    is_bool_literal,
    eof,
    whitespace,
    delimiters
) where

import qualified Data.Char as Char

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

whitespace :: [Char]
whitespace = [' ', '\n', '\r', '\t']

delimiters :: [Char]
delimiters = ['(', ')', '[', ']', eof] ++ whitespace

get_next_non_whitespace :: String -> (Char, String)
get_next_non_whitespace remaining =
    let (current, post_remaining) = get_next_character remaining
    in
        if current `elem` whitespace then
            get_next_non_whitespace post_remaining
        else
            (current, post_remaining)

get_next_character :: String -> (Char, String)
get_next_character [] = (eof, [eof])
get_next_character [current] = (current, [eof])
get_next_character (current: rest) = (current, rest)
