module Util (
    is_int_literal,
    is_bool_literal,
    map_many_on_one
) where

import qualified Data.Char as Char

bools :: [String]
bools = ["true", "false"]

is_int_literal :: String -> Bool
is_int_literal ('-':string) =
    are_digits string
is_int_literal string =
    are_digits string

are_digits :: String -> Bool
are_digits [] = False
are_digits string =
    all Char.isDigit string

is_bool_literal :: String -> Bool
is_bool_literal string =
    string `elem` bools

map_many_on_one :: [(a -> b)] -> a -> [b]
map_many_on_one [] _ = []
map_many_on_one (function: function_list) input =
    function input: map_many_on_one function_list input
