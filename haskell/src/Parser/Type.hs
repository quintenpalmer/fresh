module Parser.Type (
    parse_type
) where

import qualified Tokens.Tokens as Tokens
import qualified AST.AST as AST

import qualified Parser.Errors as Errors
import qualified Parser.Chomper as Chomper

type TokenEater = [Tokens.Token] -> AST.Environment -> (AST.Node, [Tokens.Token], AST.Environment)

parse_type :: TokenEater
parse_type [] _ = error $ Errors.unexpected_eof "type expression"
parse_type input_tokens env =
    let ((Tokens.Token token_type string file_info):tokens) = Chomper.chomp_open_expression input_tokens
    in
        case token_type of
            Tokens.StructLiteral -> parse_struct tokens env
            _ -> error $ "expecting type definition, got " ++ string ++ " " ++ show file_info

parse_struct :: TokenEater
parse_struct [] _ = error $ Errors.unexpected_eof "struct"
parse_struct tokens@((Tokens.Token _ _ file_info):_) env =
    let (members, tokens2) = Chomper.parse_fields [] tokens
        tokens3 = Chomper.chomp_close_expression tokens2 "struct"
    in
        (AST.Node (AST.StructDeclarationNode members) file_info, tokens3, env)
