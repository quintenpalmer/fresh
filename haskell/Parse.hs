module Parse (
    parse
) where

import qualified Tokenize
import qualified AST
import qualified Util

parse :: String -> AST.Node
parse raw_string =
    let (node, tokens) = parse_expression (Tokenize.to_tokens raw_string)
    in
        node

parse_expression :: [String] -> (AST.Node, [String])
parse_expression [] = error "Hi Eric and Quinten"
parse_expression (current:tokens)
    | Util.is_int_literal current = (AST.IntNode (read current), tokens)
    | Util.is_bool_literal current   = (AST.BoolNode (current == "true"), tokens)
    | otherwise = error "only ints and bools supported"

