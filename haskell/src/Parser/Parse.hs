module Parser.Parse (
    parse
) where

import qualified Tokens.Tokens as Tokens
import qualified AST.AST as AST

import qualified Parser.Definitions as Definitions
import qualified Parser.Chomper as Chomper

parse :: [Tokens.Token] -> AST.Environment -> AST.Environment
parse input_tokens input_env =
    let (tokens, env) = Definitions.parse_package_def input_tokens input_env
        (tokens1, env1) = parse_all_top_levels tokens env
    in
        case tokens1 of
            [] -> env1
            _ -> error $ "remaining tokens" ++ Tokens.print_tokens tokens1

parse_all_top_levels :: [Tokens.Token] -> AST.Environment -> ([Tokens.Token], AST.Environment)
parse_all_top_levels input_tokens input_env =
    let (tokens,env) = parse_top_level_expression input_tokens input_env
    in
        case tokens of
            [] -> (tokens, env)
            _ -> parse_all_top_levels tokens env

parse_top_level_expression :: [Tokens.Token] -> AST.Environment -> ([Tokens.Token], AST.Environment)
parse_top_level_expression [] env = ([], env)
parse_top_level_expression input_tokens env =
    let ((Tokens.Token token_type string file_info):tokens) = Chomper.chomp_open_expression input_tokens
    in
        case token_type of
            Tokens.VarLiteral -> Definitions.parse_var_def tokens env
            Tokens.TypeLiteral -> Definitions.parse_type_def tokens env
            Tokens.FunctionLiteral -> Definitions.parse_function_def tokens env
            _ -> error $ "Top level declaration must be a definition (found " ++ string ++ " ) " ++ show file_info
