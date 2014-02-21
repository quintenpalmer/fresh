module Parser.TypeDef (
    parse_type_def
) where

import qualified Data.Map as Map

import qualified Lexer.Tokenize as Tok
import qualified AST.AST as AST

import qualified Parser.Chomper as Chomper

type Token = Tok.Token
type TokenEater = [Token] -> AST.Environment -> (AST.Node, [Token], AST.Environment)

unexpected_eof :: String -> String
unexpected_eof name = "Unexpected end of file while parsing " ++ name

parse_type_def :: [Token] -> AST.Environment -> ([Token], AST.Environment)
parse_type_def [] _ = error $ unexpected_eof "type definition"
parse_type_def ((Tok.Token token_type name file_info): tokens) env =
    case token_type of
        Tok.String_ ->
            let (type_def, tokens1, env1) = parse_type_expression tokens env
                tokens2 = Chomper.chomp_close_expression tokens1 "type"
            in
                (tokens2, Map.insert name type_def env1)
        _ -> error $ "Expecting type name and defined type " ++ show file_info

parse_type_expression :: TokenEater
parse_type_expression [] _ = error $ unexpected_eof "type expression"
parse_type_expression input_tokens env =
    let ((Tok.Token token_type string file_info):tokens) = Chomper.chomp_open_expression input_tokens
    in
        case token_type of
            Tok.StructLiteral -> parse_struct tokens env
            _ -> error $ "expecting type definition, got " ++ string ++ " " ++ show file_info

parse_struct :: TokenEater
parse_struct [] _ = error "Reached end of tokens parsing struct"
parse_struct tokens@((Tok.Token _ _ file_info):_) env =
    let (members, tokens2) = Chomper.parse_fields [] tokens
        tokens3 = Chomper.chomp_close_expression tokens2 "struct"
    in
        (AST.Node (AST.StructDeclarationNode members) file_info, tokens3, env)
