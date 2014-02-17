module Lexer.Literals (
    is_int_literal,
    is_bool_literal,
    is_null_literal,
) where

import qualified Data.Char as Char

bools :: [String]
bools =
    ["true", "false"]

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

are_digits :: String -> Bool
are_digits string =
    case string of
        [] -> False
        _ -> all Char.isDigit string
