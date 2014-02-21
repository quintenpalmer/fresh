module Lexer.Literals (
    is_int_literal,
    is_bool_literal,
    is_null_literal,
    is_open_expression,
    is_close_expression,
    is_open_arguments,
    is_close_arguments
) where

import qualified Data.Char as Char

is_open_expression :: String -> Bool
is_open_expression string =
    string == "("

is_close_expression :: String -> Bool
is_close_expression string =
    string == ")"

is_open_arguments :: String -> Bool
is_open_arguments string =
    string == "["

is_close_arguments :: String -> Bool
is_close_arguments string =
    string == "]"

is_null_literal :: String -> Bool
is_null_literal string =
    string == "null"

is_bool_literal :: String -> Bool
is_bool_literal string =
    string `elem` bools

is_int_literal :: String -> Bool
is_int_literal string =
    case string of
        ('-':rest) -> are_digits rest
        _ -> are_digits string

bools :: [String]
bools =
    ["true", "false"]

are_digits :: String -> Bool
are_digits string =
    case string of
        [] -> False
        _ -> all Char.isDigit string
