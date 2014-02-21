module Parser.Parse (
    parse
) where

import qualified Data.Map as Map

import qualified Lexer.Tokenize as Tok
import qualified AST.AST as AST

import qualified Parser.Errors as Errors
import qualified Parser.Expression as Expression
import qualified Parser.TypeDef as TypeDef
import qualified Parser.FunctionDef as FunctionDef
import qualified Parser.Chomper as Chomper


type Token = Tok.Token

parse :: String -> AST.Environment -> AST.Environment
parse raw_string input_env =
    let (tokens, env) = parse_package_definition (Tok.make_tokens raw_string) input_env
        (tokens1, env1) = parse_all_top_levels tokens env
    in
        case tokens1 of
            [] -> env1
            _ -> error $ "remaining tokens" ++ Tok.print_tokens tokens1

parse_package_definition :: [Token] -> AST.Environment -> ([Token], AST.Environment)
parse_package_definition [] _ = error $ Errors.unexpected_eof "package"
parse_package_definition input_tokens env =
    let ((Tok.Token token_type string file_info):tokens) = Chomper.chomp_open_expression input_tokens
    in
        case token_type of
            Tok.PackageLiteral ->
                let (name, tokens2) = Chomper.parse_name tokens
                    tokens3 = Chomper.chomp_close_expression tokens2 "package"
                in
                    (tokens3, Map.insert name (AST.Node (AST.ModuleDefinitionNode) file_info) env)
            _ -> error $ "Top level declaration must be a package (found " ++ string ++ " ) " ++ show file_info

parse_all_top_levels :: [Token] -> AST.Environment -> ([Token], AST.Environment)
parse_all_top_levels input_tokens input_env =
    let (tokens,env) = parse_top_level_expression input_tokens input_env
    in
        case tokens of
            [] -> (tokens, env)
            _ -> parse_all_top_levels tokens env

parse_top_level_expression :: [Token] -> AST.Environment -> ([Token], AST.Environment)
parse_top_level_expression [] env = ([], env)
parse_top_level_expression input_tokens env =
    let ((Tok.Token token_type string file_info):tokens) = Chomper.chomp_open_expression input_tokens
    in
        case token_type of
            Tok.VarLiteral -> Expression.parse_var_def tokens env
            Tok.TypeLiteral -> TypeDef.parse_type_def tokens env
            Tok.FunctionLiteral -> FunctionDef.parse_function_def tokens env
            _ -> error $ "Top level declaration must be a define (found " ++ string ++ " ) " ++ show file_info
