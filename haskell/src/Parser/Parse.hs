module Parser.Parse (
    parse
) where

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import qualified Lexer.Tokenize as Tok
import qualified AST.AST as AST

import qualified Parser.Builtin as Builtin


type Token = Tok.Token

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
                Builtin.parse_define tokens env
            else
                error $ "Top level declaration must be a define" ++ show file_info
        _ -> error $ "Top level declaration must be a define" ++ show file_info
