module Parser.Definitions (
    parse_var_def,
    parse_function_def,
    parse_type_def,
    parse_package_def
) where

import qualified Data.Map as Map

import qualified Lexer.Tokenize as Tok
import qualified AST.AST as AST

import qualified Parser.Errors as Errors
import qualified Parser.Chomper as Chomper

import qualified Parser.Expression as Expression
import qualified Parser.Type as Type

parse_package_def :: [Tok.Token] -> AST.Environment -> ([Tok.Token], AST.Environment)
parse_package_def [] _ = error $ Errors.unexpected_eof "package"
parse_package_def input_tokens env =
    let ((Tok.Token token_type string file_info):tokens) = Chomper.chomp_open_expression input_tokens
    in
        case token_type of
            Tok.PackageLiteral ->
                let (name, tokens2) = Chomper.parse_name tokens
                    tokens3 = Chomper.chomp_close_expression tokens2 "package"
                in
                    (tokens3, Map.insert name (AST.Node (AST.ModuleDefinitionNode) file_info) env)
            _ -> error $ "Top level declaration must be a package (found " ++ string ++ " ) " ++ show file_info

parse_var_def :: [Tok.Token] -> AST.Environment -> ([Tok.Token], AST.Environment)
parse_var_def [] _ = error $ Errors.unexpected_eof "var"
parse_var_def ((Tok.Token token_type name _):tokens) env =
    case token_type of
        Tok.String_ ->
            let (expression, tokens1, env1) = Expression.parse_expression tokens env
                tokens2 = Chomper.chomp_close_expression tokens1 "var"
            in
                (tokens2, Map.insert name expression env1)
        token -> error $ show token

parse_function_def :: [Tok.Token] -> AST.Environment -> ([Tok.Token], AST.Environment)
parse_function_def [] _ = error $ Errors.unexpected_eof "function definition"
parse_function_def ((Tok.Token token_function name file_info): tokens) env =
    case token_function of
        Tok.String_ ->
            let (function_def, tokens1, env1) = Expression.parse_lambda tokens env
            in
                (tokens1, Map.insert name function_def env1)
        _ -> error $ "Expecting function name and defined function " ++ show file_info

parse_type_def :: [Tok.Token] -> AST.Environment -> ([Tok.Token], AST.Environment)
parse_type_def [] _ = error $ Errors.unexpected_eof "type definition"
parse_type_def ((Tok.Token token_type name file_info): tokens) env =
    case token_type of
        Tok.String_ ->
            let (type_def, tokens1, env1) = Type.parse_type tokens env
                tokens2 = Chomper.chomp_close_expression tokens1 "type"
            in
                (tokens2, Map.insert name type_def env1)
        _ -> error $ "Expecting type name and defined type " ++ show file_info
