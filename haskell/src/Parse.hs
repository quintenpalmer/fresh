module Parse (
    parse
) where

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import qualified Tokenize
import qualified AST

type Token = Tokenize.Token

parse :: String -> AST.Node
parse raw_string =
    let (node, tokens) = parse_expression (Tokenize.to_tokens raw_string)
    in
        node

parse_expression :: [Token] -> (AST.Node, [Token])
parse_expression [] = error "Hi Eric and Quinten"
parse_expression ((Tokenize.LParen):tokens) = parse_func_call tokens
parse_expression ((Tokenize.IntLiteral int):tokens) = (AST.IntNode int, tokens)
parse_expression ((Tokenize.BoolLiteral bool):tokens) = (AST.BoolNode bool, tokens)
parse_expression ((Tokenize.String_ name):tokens) = (AST.VariableNode name, tokens)

parse_func_call :: [Token] -> (AST.Node, [Token])
parse_func_call ((Tokenize.String_ current):tokens) =
    let maybe_function = Map.lookup current function_map
    in
        if Maybe.isJust maybe_function then
            (Maybe.fromJust maybe_function) tokens
        else
            let (operands, pre_close_tokens) = parse_operands [] tokens
                post_close_tokens = chomp_close_expression pre_close_tokens
            in
                (AST.FunctionCallNode current operands, post_close_tokens)
parse_func_call (token:tokens) =
    error $ show token

parse_if :: [Token] -> (AST.Node, [Token])
parse_if tokens =
    let (cond_expr, tokens1) = parse_expression tokens
        (then_expr, tokens2) = parse_expression tokens1
        (else_expr, tokens3) = parse_expression tokens2
        tokens4 = chomp_close_expression tokens3
    in
        (AST.IfNode cond_expr then_expr else_expr, tokens4)

parse_define :: [Token] -> (AST.Node, [Token])
parse_define ((Tokenize.String_ name): tokens) =
    let (expression, tokens1) = parse_expression tokens
        tokens2 = chomp_close_expression tokens1
        (body, tokens3) = parse_expression tokens2
    in
        (AST.BindingNode name expression body, tokens3)
parse_define (token:tokens) =
    error $ show token

parse_lambda :: [Token] -> (AST.Node, [Token])
parse_lambda tokens =
    let tokens1 = chomp_open_lambda_params tokens
        (params, tokens2) = parse_params [] tokens1
        tokens3 = chomp_close_lambda_params tokens2
        (body, tokens4) = parse_expression tokens3
        tokens5 = chomp_close_expression tokens4
    in
        (AST.LambdaNode body params, tokens5)


parse_params :: [String] -> [Token] -> ([String], [Token])
parse_params existing_params input_tokens@((Tokenize.RBracket):_) =
    (existing_params, input_tokens)
parse_params existing_params ((Tokenize.String_ name):tokens) =
    parse_params (name: existing_params) tokens


parse_operands :: [AST.Node] -> [Token] -> ([AST.Node], [Token])
parse_operands existing_operands input_tokens@((Tokenize.RParen):_) =
    (existing_operands, input_tokens)
parse_operands existing_operands input_tokens =
    let (current, remaining_tokens) = parse_expression input_tokens
    in
        parse_operands (current: existing_operands) remaining_tokens


chomp_close_expression :: [Token] -> [Token]
chomp_close_expression tokens =
    assert_chomping Tokenize.RParen tokens

chomp_open_lambda_params :: [Token] -> [Token]
chomp_open_lambda_params tokens =
    assert_chomping Tokenize.LBracket tokens

chomp_close_lambda_params :: [Token] -> [Token]
chomp_close_lambda_params tokens =
    assert_chomping Tokenize.RBracket tokens

assert_chomping :: Token -> [Token] -> [Token]
assert_chomping expected (token:tokens) =
    if token == expected then
        tokens
    else
        error $ "wrong token'" ++ (show expected) ++ "', found '" ++ (show token) ++ "'"

function_map :: Map.Map String ([Token] -> (AST.Node, [Token]))
function_map = Map.fromList [
    ("if", parse_if),
    ("define", parse_define),
    ("lambda", parse_lambda)]
