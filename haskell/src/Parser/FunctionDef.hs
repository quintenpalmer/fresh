module Parser.FunctionDef (
    parse_function_def
) where

import qualified Data.Map as Map

import qualified Lexer.Tokenize as Tok
import qualified AST.AST as AST

import qualified Parser.Chomper as Chomper
import qualified Parser.Expression as Expression

type Token = Tok.Token
type TokenEater = [Token] -> AST.Environment -> (AST.NodeContainer, [Token], AST.Environment)

unexpected_eof :: String -> String
unexpected_eof name = "Unexpected end of file while parsing " ++ name

parse_function_def :: [Token] -> AST.Environment -> ([Token], AST.Environment)
parse_function_def [] _ = error $ unexpected_eof "function definition"
parse_function_def ((Tok.Token token_function name file_info): tokens) env =
    case token_function of
        Tok.String_ ->
            let (function_def, tokens1, env1) = parse_function_expression tokens env
                tokens2 = Chomper.chomp_close_expression tokens1 "function"
            in
                (tokens2, Map.insert name function_def env1)
        _ -> error $ "Expecting function name and defined function " ++ show file_info

parse_function_expression :: TokenEater
parse_function_expression [] _ = error $ unexpected_eof "function expression"
parse_function_expression tokens@((Tok.Token _ _ file_info):_) env =
    let tokens1 = Chomper.chomp_open_lambda_params tokens
        (params, tokens2) = Chomper.parse_params [] tokens1
        tokens3 = Chomper.chomp_close_lambda_params tokens2
        (body, tokens4, env1) = Expression.parse_expression tokens3 env
    in
        (AST.NodeContainer (AST.LambdaNode body params) file_info, tokens4, env1)

