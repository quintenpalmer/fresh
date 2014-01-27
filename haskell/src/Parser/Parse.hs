module Parser.Parse (
    parse
) where

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import qualified Lexer.Tokenize as Tok
import qualified Parser.AST as AST

type Token = Tok.Token
type TokenEater = [Token] -> AST.Environment -> (AST.NodeContainer, [Token], AST.Environment)

parse :: String -> AST.Environment -> (AST.NodeContainer, AST.Environment)
parse raw_string input_env =
    let (tokens, env) = parse_all_top_levels (Tok.make_tokens raw_string) input_env
        out_node = parse_main env
    in
        case tokens of
            [] -> (out_node, env)
            [(Tok.Token (Tok.Eof) _ _)] -> (out_node, env)
            _ -> error $ "remaining tokens" ++ show tokens

parse_main :: AST.Environment -> AST.NodeContainer
parse_main env =
    let maybe_main = Map.lookup "main" env
    in
        if Maybe.isJust maybe_main then
            Maybe.fromJust maybe_main
        else
            error $ "main function not defined"

parse_all_top_levels :: [Token] -> AST.Environment -> ([Token], AST.Environment)
parse_all_top_levels input_tokens input_env =
    let (tokens,env) = parse_top_level_expression input_tokens input_env
    in
        case tokens of
            [] -> (tokens, env)
            [(Tok.Token (Tok.Eof) _ _)] -> (tokens, env)
            _ -> parse_all_top_levels tokens env

parse_top_level_expression :: [Token] -> AST.Environment -> ([Token], AST.Environment)
parse_top_level_expression [] env = ([], env)
parse_top_level_expression ((Tok.Token token_type _ file_info):tokens) env =
    case token_type of
        Tok.LParen -> parse_better_be_define tokens env
        _ -> error $ "must be a definition of define at top level" ++ show file_info

parse_better_be_define :: [Token] -> AST.Environment -> ([Token], AST.Environment)
parse_better_be_define [] _ = error "Top level declarations must be a define"
parse_better_be_define ((Tok.Token token_type string file_info):tokens) env =
    case token_type of
        Tok.String_ ->
            if string == "define" then
                parse_define tokens env
            else
                error $ "Top level declaration must be a define" ++ show file_info
        _ -> error $ "Top level declaration must be a define" ++ show file_info

parse_expression :: TokenEater
parse_expression [] _ = error "Hi Eric and Quinten"
parse_expression ((Tok.Token token_type string file_info):tokens) env =
    case token_type of
        Tok.LParen -> parse_func_call tokens env
        Tok.IntLiteral -> (AST.NodeContainer (AST.IntNode $ read string) file_info, tokens, env)
        Tok.BoolLiteral -> (AST.NodeContainer (AST.BoolNode $ string == "true") file_info, tokens, env)
        Tok.String_ -> (AST.NodeContainer (AST.VariableNode string) file_info, tokens, env)
        token -> error $ "Invalid Token " ++ show token

parse_func_call :: TokenEater
parse_func_call [] _ = error "Unexpected end of tokens in parse function call"
parse_func_call ((Tok.Token token_type current file_info):tokens) env =
    case token_type of
        Tok.String_ ->
            let maybe_function = Map.lookup current function_map
            in
                if Maybe.isJust maybe_function then
                    (Maybe.fromJust maybe_function) tokens env
                else
                    let (operands, pre_close_tokens) = parse_operands [] tokens env
                        post_close_tokens = chomp_close_expression pre_close_tokens
                    in
                        (AST.NodeContainer (AST.FunctionCallNode current operands) file_info, post_close_tokens, env)
        token -> error $ show token

parse_define :: [Token] -> AST.Environment -> ([Token], AST.Environment)
parse_define [] _ = error "Unexpected end of tokens in parse define"
parse_define ((Tok.Token token_type name _):tokens) env =
    case token_type of
        Tok.String_ ->
            let (expression, tokens1, env1) = parse_expression tokens env
                tokens2 = chomp_close_expression tokens1
            in
                (tokens2, Map.insert name expression env1)
        token -> error $ show token

parse_if :: TokenEater
parse_if [] _ = error "Reached end of tokens parsing if"
parse_if tokens@((Tok.Token _ _ file_info):_) env =
    let (cond_expr, tokens1, env1) = parse_expression tokens env
        (then_expr, tokens2, env2) = parse_expression tokens1 env1
        (else_expr, tokens3, env3) = parse_expression tokens2 env2
        tokens4 = chomp_close_expression tokens3
    in
        (AST.NodeContainer (AST.IfNode cond_expr then_expr else_expr) file_info, tokens4, env3)

parse_lambda :: TokenEater
parse_lambda [] _ = error "Reached end of tokens parsing lambda"
parse_lambda tokens@((Tok.Token _ _ file_info):_) env =
    let tokens1 = chomp_open_lambda_params tokens
        (params, tokens2) = parse_params [] tokens1
        tokens3 = chomp_close_lambda_params tokens2
        (body, tokens4, env1) = parse_expression tokens3 env
        tokens5 = chomp_close_expression tokens4
    in
        (AST.NodeContainer (AST.LambdaNode body params) file_info, tokens5, env1)

parse_struct :: TokenEater
parse_struct [] _ = error "Reached end of tokens parsing struct"
parse_struct tokens@((Tok.Token _ _ file_info):_) env =
    let (members, tokens2) = parse_fields [] tokens
        tokens3 = chomp_close_expression tokens2
    in
        (AST.NodeContainer (AST.StructDeclarationNode members) file_info, tokens3, env)

parse_member :: TokenEater
parse_member input_tokens env =
    case input_tokens of
        ((Tok.Token Tok.String_ struct_name file_info):((Tok.Token Tok.String_ member_name _):tokens)) ->
            let remaining_tokens = chomp_close_expression tokens
            in
                (AST.NodeContainer (AST.MemberAccessNode struct_name member_name) file_info, remaining_tokens, env)
        _ -> error "member must take a struct name and a member name"

parse_fields :: [String] -> [Token] -> ([String], [Token])
parse_fields _ [] = error "No fields to parse in parse_fields"
parse_fields existing_params input_tokens@((Tok.Token token_type name _):tokens) =
    case token_type of
        Tok.RParen -> (existing_params, input_tokens)
        Tok.String_ -> parse_fields (name: existing_params) tokens
        _ -> error $ "Was expecting parameter or ] when found " ++ name

parse_params :: [String] -> [Token] -> ([String], [Token])
parse_params _ [] = error "No parameters to parse in parse_params"
parse_params existing_params input_tokens@((Tok.Token token_type name _):tokens) =
    case token_type of
        Tok.RBracket -> (existing_params, input_tokens)
        Tok.String_ -> parse_params (name: existing_params) tokens
        _ -> error $ "Was expecting parameter or ] when found " ++ name


parse_operands :: [AST.NodeContainer] -> [Token] -> AST.Environment -> ([AST.NodeContainer], [Token])
parse_operands _ [] _ = error "No operands to parse in parse_operands"
parse_operands existing_params input_tokens@((Tok.Token token_type _ _):_) env =
    case token_type of
        Tok.RParen -> (existing_params, input_tokens)
        _ ->
            let (current, remaining_tokens, env1) = parse_expression input_tokens env
            in
                parse_operands (current: existing_params) remaining_tokens env1

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
assert_chomping expected_token_type ((Tok.Token token_type string _):tokens) =
    if token_type == expected_token_type then
        tokens
    else
        error $ "wrong token'" ++ (show expected_token_type) ++ "', found '" ++ string ++ "'"

function_map :: Map.Map String TokenEater
function_map = Map.fromList [
    ("if", parse_if),
    ("lambda", parse_lambda),
    ("struct", parse_struct),
    ("member", parse_member)]
