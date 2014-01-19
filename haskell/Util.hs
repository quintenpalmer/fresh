module Util (
    is_int_literal,
    is_bool_literal,
    map_many_on_one
) where

import qualified Data.Char as Char

bools = ["true", "false"]

is_int_literal string =
    if head string == '-' then
        are_digits $ tail string
    else
        are_digits string

are_digits string =
    all Char.isDigit string

is_bool_literal string =
    string `elem` bools

map_many_on_one :: [(a -> b)] -> a -> [b]
map_many_on_one [] _ = []
map_many_on_one (function: function_list) input =
    function input: map_many_on_one function_list input