module Lexer.Tokenize (
    Token(..),
    TokenType(..),
    to_tokens
) where

import qualified Lexer.StringParse as StringParse

data Token = Token TokenType String deriving (Show)

data TokenType
    = IntLiteral
    | BoolLiteral
    | String_
    | Eof
    | LParen
    | RParen
    | LBracket
    | RBracket deriving (Show, Eq)


to_tokens :: String -> [Token]
to_tokens input_string =
    if (head input_string) == StringParse.eof then
        []
    else
        let (token, remaining) = get_token input_string
        in
            token: to_tokens remaining

get_token :: String -> (Token, String)
get_token remaining =
    let (current, post_remaining) = StringParse.get_next_non_whitespace remaining
    in
        if current `elem` StringParse.delimiters then
            (convert_token [current], post_remaining)
        else
            build_next_token [current] post_remaining

convert_token :: String -> Token
convert_token current =
    Token (get_token_type current) current

get_token_type :: String -> TokenType
get_token_type current
    | current == "(" = LParen
    | current == ")" = RParen
    | current == "[" = LBracket
    | current == "]" = RBracket
    | current == [StringParse.eof] = Eof
    | StringParse.is_int_literal current = IntLiteral
    | StringParse.is_bool_literal current   = BoolLiteral
    | otherwise = String_


build_next_token :: String -> String -> (Token, String)
build_next_token input_token remaining =
    let (current, post_remaining) = StringParse.get_next_character remaining
    in
        if current `elem` StringParse.delimiters then
            (convert_token input_token, remaining)
        else
            build_next_token (input_token ++ [current]) post_remaining
