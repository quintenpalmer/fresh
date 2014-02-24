module Lexer.Literals (
    is_type,
    is_type_delimiter,
    is_int_literal,
    is_bool_literal,
    is_null_literal,
    is_if_literal,
    is_lambda_literal,
    is_member_literal,
    is_var_literal,
    is_type_literal,
    is_function_literal,
    is_struct_literal,
    is_package_literal,
    is_open_expression,
    is_close_expression,
    is_open_arguments,
    is_close_arguments
) where

import qualified Data.Char as Char

is_type :: String -> Bool
is_type [] = False
is_type (first:_) =
    Char.isUpper first

is_type_delimiter :: String -> Bool
is_type_delimiter string =
    string == ":"

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

is_lambda_literal :: String -> Bool
is_lambda_literal string =
    string == "lambda"

is_if_literal :: String -> Bool
is_if_literal string =
    string == "if"

is_member_literal :: String -> Bool
is_member_literal string =
    string == "member"

is_var_literal :: String -> Bool
is_var_literal string =
    string == "var"

is_function_literal :: String -> Bool
is_function_literal string =
    string == "function"

is_type_literal :: String -> Bool
is_type_literal string =
    string == "type"

is_struct_literal :: String -> Bool
is_struct_literal string =
    string == "struct"

is_package_literal :: String -> Bool
is_package_literal string =
    string == "package"

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
