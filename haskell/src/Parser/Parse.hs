module Parser.Parse (
    parse
) where

import qualified Lexer.Tokenize as Tok
import qualified AST.AST as AST

import qualified Parser.Definitions as Definitions
import qualified Parser.Chomper as Chomper

parse :: [Tok.Token] -> AST.Environment -> AST.Environment
parse input_tokens input_env =
    let (tokens, env) = Definitions.parse_package_def input_tokens input_env
        (tokens1, env1) = parse_all_top_levels tokens env
    in
        case tokens1 of
            [] -> env1
            _ -> error $ "remaining tokens" ++ Tok.print_tokens tokens1

parse_all_top_levels :: [Tok.Token] -> AST.Environment -> ([Tok.Token], AST.Environment)
parse_all_top_levels input_tokens input_env =
    let (tokens,env) = parse_top_level_expression input_tokens input_env
    in
        case tokens of
            [] -> (tokens, env)
            _ -> parse_all_top_levels tokens env

parse_top_level_expression :: [Tok.Token] -> AST.Environment -> ([Tok.Token], AST.Environment)
parse_top_level_expression [] env = ([], env)
parse_top_level_expression input_tokens env =
    let ((Tok.Token token_type string file_info):tokens) = Chomper.chomp_open_expression input_tokens
    in
        case token_type of
            Tok.VarLiteral -> Definitions.parse_var_def tokens env
            Tok.TypeLiteral -> Definitions.parse_type_def tokens env
            Tok.FunctionLiteral -> Definitions.parse_function_def tokens env
            _ -> error $ "Top level declaration must be a definition (found " ++ string ++ " ) " ++ show file_info
