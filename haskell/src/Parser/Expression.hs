module Parser.Expression (
    parse_expression,
    parse_lambda
) where

import qualified Tokens.Tokens as Tokens
import qualified AST.AST as AST

import qualified Parser.Errors as Errors
import qualified Parser.Chomper as Chomper

type TokenEater = [Tokens.Token] -> AST.Environment -> (AST.Node, [Tokens.Token], AST.Environment)

parse_expression :: TokenEater
parse_expression [] _ = error $ Errors.unexpected_eof "general expression"
parse_expression ((Tokens.Token token_type string file_info):tokens) env =
    case token_type of
        Tokens.LParen -> parse_func_call tokens env
        Tokens.IntLiteral -> (AST.Node (AST.IntNode $ read string) file_info, tokens, env)
        Tokens.BoolLiteral -> (AST.Node (AST.BoolNode $ string == "true") file_info, tokens, env)
        Tokens.NullLiteral -> (AST.Node (AST.NullNode) file_info, tokens, env)
        Tokens.String_ -> (AST.Node (AST.VariableNode string) file_info, tokens, env)
        token -> error $ "Invalid Token to start expression " ++ show token ++ " " ++ show file_info

parse_func_call :: TokenEater
parse_func_call [] _ = error $ Errors.unexpected_eof "function call"
parse_func_call ((Tokens.Token token_type current file_info):tokens) env =
    case token_type of
        Tokens.String_ ->
            let (operands, pre_close_tokens) = parse_operands [] tokens env
                post_close_tokens = Chomper.chomp_close_expression pre_close_tokens "func_call"
            in
                (AST.Node (AST.FunctionCallNode current operands) file_info, post_close_tokens, env)
        Tokens.Type ->
            let (operands, pre_close_tokens) = parse_operands [] tokens env
                post_close_tokens = Chomper.chomp_close_expression pre_close_tokens "func_call"
            in
                (AST.Node (AST.FunctionCallNode current operands) file_info, post_close_tokens, env)
        Tokens.LambdaLiteral -> parse_lambda tokens env
        Tokens.IfLiteral -> parse_if tokens env
        Tokens.MemberLiteral -> parse_member tokens env
        token -> error $ "Invalid token as func call " ++ show token ++ " " ++ show file_info

parse_operands :: [AST.Node] -> [Tokens.Token] -> AST.Environment -> ([AST.Node], [Tokens.Token])
parse_operands _ [] _ = error $ Errors.unexpected_eof "parsing of operands"
parse_operands existing_params input_tokens@((Tokens.Token token_type _ _):_) env =
    case token_type of
        Tokens.RParen -> (existing_params, input_tokens)
        _ ->
            let (current, remaining_tokens, env1) = parse_expression input_tokens env
            in
                parse_operands (current: existing_params) remaining_tokens env1

parse_if :: TokenEater
parse_if [] _ = error $ Errors.unexpected_eof "if"
parse_if tokens@((Tokens.Token _ _ file_info):_) env =
    let (cond_expr, tokens1, env1) = parse_expression tokens env
        (then_expr, tokens2, env2) = parse_expression tokens1 env1
        (else_expr, tokens3, env3) = parse_expression tokens2 env2
        tokens4 = Chomper.chomp_close_expression tokens3 "if"
    in
        (AST.Node (AST.IfNode cond_expr then_expr else_expr) file_info, tokens4, env3)

parse_lambda :: TokenEater
parse_lambda [] _ = error $ Errors.unexpected_eof "lambda"
parse_lambda tokens@((Tokens.Token _ _ file_info):_) env =
    let tokens1 = Chomper.chomp_open_lambda_params tokens
        (params, tokens2) = Chomper.parse_params [] tokens1
        tokens3 = Chomper.chomp_close_lambda_params tokens2
        (body, tokens4, env1) = parse_expression tokens3 env
        tokens5 = Chomper.chomp_close_expression tokens4 "lambda"
    in
        (AST.Node (AST.LambdaNode body params) file_info, tokens5, env1)

parse_member :: TokenEater
parse_member [] _ = error $ Errors.unexpected_eof "member"
parse_member input_tokens env =
    case input_tokens of
        ((Tokens.Token Tokens.String_ struct_name file_info):((Tokens.Token Tokens.String_ member_name _):tokens)) ->
            let remaining_tokens = Chomper.chomp_close_expression tokens "member"
            in
                (AST.Node (AST.MemberAccessNode struct_name member_name) file_info, remaining_tokens, env)
        _ -> error "member must take a struct name and a member name"
