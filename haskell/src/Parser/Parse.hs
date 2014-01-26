module Parser.Parse (
    parse
) where

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import qualified Lexer.Tokenize as Tok
import qualified Parser.AST as AST

type Token = Tok.Token

parse :: String -> AST.Node
parse raw_string =
    let (node, tokens) = parse_expression (Tok.to_tokens raw_string)
    in
        case tokens of
            [] -> node
            [(Tok.Token (Tok.Eof) _)] -> node
            _ -> error $ "remaining tokens" ++ show tokens

parse_expression :: [Token] -> (AST.Node, [Token])
parse_expression [] = error "Hi Eric and Quinten"
parse_expression ((Tok.Token token_type string):tokens) =
    case token_type of
        Tok.LParen -> parse_func_call tokens
        Tok.IntLiteral -> (AST.IntNode $ read string, tokens)
        Tok.BoolLiteral -> (AST.BoolNode $ string == "true", tokens)
        Tok.String_ -> (AST.VariableNode string, tokens)
        token -> error $ "Invalid Token " ++ show token

parse_func_call :: [Token] -> (AST.Node, [Token])
parse_func_call [] = error "Unexpected end of tokens in parse function call"
parse_func_call ((Tok.Token token_type current):tokens) =
    case token_type of
        Tok.String_ ->
            let maybe_function = Map.lookup current function_map
            in
                if Maybe.isJust maybe_function then
                    (Maybe.fromJust maybe_function) tokens
                else
                    let (operands, pre_close_tokens) = parse_operands [] tokens
                        post_close_tokens = chomp_close_expression pre_close_tokens
                    in
                        (AST.FunctionCallNode current operands, post_close_tokens)
        token -> error $ show token

parse_if :: [Token] -> (AST.Node, [Token])
parse_if tokens =
    let (cond_expr, tokens1) = parse_expression tokens
        (then_expr, tokens2) = parse_expression tokens1
        (else_expr, tokens3) = parse_expression tokens2
        tokens4 = chomp_close_expression tokens3
    in
        (AST.IfNode cond_expr then_expr else_expr, tokens4)

parse_define :: [Token] -> (AST.Node, [Token])
parse_define [] = error "Unexpected end of tokens in parse define"
parse_define ((Tok.Token token_type name):tokens) =
    case token_type of
        Tok.String_ ->
            let (expression, tokens1) = parse_expression tokens
                tokens2 = chomp_close_expression tokens1
                (body, tokens3) = parse_expression tokens2
            in
                (AST.BindingNode name expression body, tokens3)
        token -> error $ show token

parse_lambda :: [Token] -> (AST.Node, [Token])
parse_lambda tokens =
    let tokens1 = chomp_open_lambda_params tokens
        (params, tokens2) = parse_params [] tokens1
        tokens3 = chomp_close_lambda_params tokens2
        (body, tokens4) = parse_expression tokens3
        tokens5 = chomp_close_expression tokens4
    in
        (AST.LambdaNode body params, tokens5)

parse_struct :: [Token] -> (AST.Node, [Token])
parse_struct tokens =
    let (members, tokens2) = parse_fields [] tokens
        tokens3 = chomp_close_expression tokens2
    in
        (AST.StructDeclarationNode members, tokens3)

parse_member :: [Token] -> (AST.Node, [Token])
parse_member input_tokens =
    case input_tokens of
        ((Tok.Token (Tok.String_) struct_name):((Tok.Token (Tok.String_) member_name):tokens)) ->
            (AST.MemberAccessNode struct_name member_name, tokens)
        _ -> error "member must take a struct name and a member name"

parse_fields :: [String] -> [Token] -> ([String], [Token])
parse_fields _ [] = error "No parameters to parse in parse_params"
parse_fields existing_params input_tokens@((Tok.Token token_type name):tokens) =
    case token_type of
        Tok.RParen -> (existing_params, input_tokens)
        Tok.String_ -> parse_fields (name: existing_params) tokens
        _ -> error $ "Was expecting parameter or ] when found " ++ name

parse_params :: [String] -> [Token] -> ([String], [Token])
parse_params _ [] = error "No parameters to parse in parse_params"
parse_params existing_params input_tokens@((Tok.Token token_type name):tokens) =
    case token_type of
        Tok.RBracket -> (existing_params, input_tokens)
        Tok.String_ -> parse_params (name: existing_params) tokens
        _ -> error $ "Was expecting parameter or ] when found " ++ name


parse_operands :: [AST.Node] -> [Token] -> ([AST.Node], [Token])
parse_operands _ [] = error "No parameters to parse in parse_operands"
parse_operands existing_params input_tokens@((Tok.Token token_type _):_) =
    case token_type of
        Tok.RParen -> (existing_params, input_tokens)
        _ ->
            let (current, remaining_tokens) = parse_expression input_tokens
            in
                parse_operands (current: existing_params) remaining_tokens

chomp_close_expression :: [Token] -> [Token]
chomp_close_expression tokens =
    assert_chomping Tok.RParen tokens

chomp_open_lambda_params :: [Token] -> [Token]
chomp_open_lambda_params tokens =
    assert_chomping Tok.LBracket tokens

chomp_close_lambda_params :: [Token] -> [Token]
chomp_close_lambda_params tokens =
    assert_chomping Tok.RBracket tokens

assert_chomping :: Tok.TokenType -> [Token] -> [Token]
assert_chomping expected [] = error $ "End of tokens when asserting for " ++ show expected
assert_chomping expected_token_type ((Tok.Token token_type string):tokens) =
    if token_type == expected_token_type then
        tokens
    else
        error $ "wrong token'" ++ (show expected_token_type) ++ "', found '" ++ string ++ "'"

function_map :: Map.Map String ([Token] -> (AST.Node, [Token]))
function_map = Map.fromList [
    ("if", parse_if),
    ("define", parse_define),
    ("lambda", parse_lambda),
    ("struct", parse_struct),
    ("member", parse_member)]
