module Parse (
    parse
) where

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

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
    | current == "(" = parse_func_call tokens
    | Util.is_int_literal current = (AST.IntNode (read current), tokens)
    | Util.is_bool_literal current   = (AST.BoolNode (current == "true"), tokens)
    | otherwise = (AST.VariableNode current, tokens)

parse_func_call :: [String] -> (AST.Node, [String])
parse_func_call (current:tokens) =
    let maybe_function = Map.lookup current function_map
    in
        if Maybe.isJust maybe_function then
            (Maybe.fromJust maybe_function) tokens
        else
            let (operands, pre_close_tokens) = parse_operands [] tokens
                post_close_tokens = chomp_close_expression pre_close_tokens
            in
                (AST.FunctionCallNode current operands, post_close_tokens)

parse_if :: [String] -> (AST.Node, [String])
parse_if tokens =
    let (cond_expr, tokens1) = parse_expression tokens
        (then_expr, tokens2) = parse_expression tokens1
        (else_expr, tokens3) = parse_expression tokens2
        tokens4 = chomp_close_expression tokens3
    in
        (AST.IfNode cond_expr then_expr else_expr, tokens4)

parse_define :: [String] -> (AST.Node, [String])
parse_define (name: tokens) =
    let (expression, tokens1) = parse_expression tokens
        tokens2 = chomp_close_expression tokens1
        (body, tokens3) = parse_expression tokens2
    in
        (AST.BindingNode name expression body, tokens3)

parse_lambda :: [String] -> (AST.Node, [String])
parse_lambda tokens =
    let tokens1 = chomp_open_lambda_params tokens
        (params, tokens2) = parse_params [] tokens1
        tokens3 = chomp_close_lambda_params tokens2
        (body, tokens4) = parse_expression tokens3
        tokens5 = chomp_close_expression tokens4
    in
        (AST.LambdaNode body params, tokens5)


parse_params :: [String] -> [String] -> ([String], [String])
parse_params existing_params input_tokens@("]":_) =
    (existing_params, input_tokens)
parse_params existing_params (token:tokens) =
    parse_params (token: existing_params) tokens


parse_operands :: [AST.Node] -> [String] -> ([AST.Node], [String])
parse_operands existing_operands input_tokens@(")":_) =
    (existing_operands, input_tokens)
parse_operands existing_operands input_tokens =
    let (current, remaining_tokens) = parse_expression input_tokens
    in
        parse_operands (current: existing_operands) remaining_tokens


chomp_close_expression :: [String] -> [String]
chomp_close_expression tokens =
    assert_chomping tokens ")"

chomp_open_lambda_params :: [String] -> [String]
chomp_open_lambda_params tokens =
    assert_chomping tokens "["

chomp_close_lambda_params :: [String] -> [String]
chomp_close_lambda_params tokens =
    assert_chomping tokens "]"

assert_chomping :: [String] -> String -> [String]
assert_chomping (token:tokens) expected =
    if token == expected then
        tokens
    else
        error $ "expecting '" ++ expected ++ "', found '" ++ token ++ "'"

function_map :: Map.Map String ([String] -> (AST.Node, [String]))
function_map = Map.fromList [
    ("if", parse_if),
    ("define", parse_define),
    ("lambda", parse_lambda)]
