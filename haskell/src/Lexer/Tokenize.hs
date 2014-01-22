module Lexer.Tokenize (
    Token(..),
    TokenType(..),
    to_tokens
) where

import qualified Util

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


eof :: Char
eof = '\0'

whitespace :: [Char]
whitespace = [' ', '\n', '\r', '\t']

delimiters :: [Char]
delimiters = ['(', ')', '[', ']', eof] ++ whitespace

to_tokens :: String -> [Token]
to_tokens input_string =
    if (head input_string) == eof then
        []
    else
        let (token, remaining) = get_token input_string
        in
            token: to_tokens remaining

get_token :: String -> (Token, String)
get_token remaining =
    let (current, post_remaining) = get_next_non_whitespace remaining
    in
        if current `elem` delimiters then
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
    | current == [eof] = Eof
    | Util.is_int_literal current = IntLiteral
    | Util.is_bool_literal current   = BoolLiteral
    | otherwise = String_


build_next_token :: String -> String -> (Token, String)
build_next_token input_token remaining =
    let (current, post_remaining) = get_next_character remaining
    in
        if current `elem` delimiters then
            (convert_token input_token, remaining)
        else
            build_next_token (input_token ++ [current]) post_remaining

get_next_non_whitespace :: String -> (Char, String)
get_next_non_whitespace remaining =
    let (current, post_remaining) = get_next_character remaining
    in
        if current `elem` whitespace then
            get_next_non_whitespace post_remaining
        else
            (current, post_remaining)

get_next_character :: String -> (Char, String)
get_next_character [] = (eof, [eof])
get_next_character [current] = (current, [eof])
get_next_character (current: rest) = (current, rest)
