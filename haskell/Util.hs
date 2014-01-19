module Util (
    is_int_literal,
    is_bool_literal
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
