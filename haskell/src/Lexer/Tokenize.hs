module Lexer.Tokenize (
    Token(..),
    TokenType(..),
    StringParse.FileLoc(..),
    StringParse.FileLocInfo(..),
    StringParse.print_file_info,
    make_tokens
) where

import qualified Lexer.StringParse as StringParse

type FileLocInfo = StringParse.FileLocInfo

data Token = Token TokenType String FileLocInfo deriving (Show)

data TokenType
    = IntLiteral
    | BoolLiteral
    | String_
    | Eof
    | LParen
    | RParen
    | LBracket
    | RBracket deriving (Show, Eq)


make_tokens :: String -> [Token]
make_tokens input_string =
    to_tokens input_string (StringParse.FileLocInfo (StringParse.FileLoc 0 0) (StringParse.FileLoc 0 0))

to_tokens :: String -> FileLocInfo -> [Token]
to_tokens input_string input_file_info =
    if (head input_string) == StringParse.eof then
        []
    else
        let (token, remaining, file_info) = get_token input_string input_file_info
        in
            token: to_tokens remaining file_info

get_token :: String -> FileLocInfo -> (Token, String, FileLocInfo)
get_token remaining input_file_info =
    let (current, post_remaining, file_info) = StringParse.get_next_non_whitespace remaining input_file_info
    in
        if current `elem` StringParse.delimiters then
            (convert_token [current] file_info, post_remaining, file_info)
        else
            build_next_token [current] post_remaining file_info

convert_token :: String -> FileLocInfo -> Token
convert_token current file_info =
    Token (get_token_type current) current file_info

build_next_token :: String -> String -> FileLocInfo -> (Token, String, FileLocInfo)
build_next_token input_token remaining input_file_info =
    let (current, post_remaining, file_info) = StringParse.get_next_character remaining input_file_info
    in
        if current `elem` StringParse.delimiters then
            (convert_token input_token file_info, remaining, file_info)
        else
            build_next_token (input_token ++ [current]) post_remaining file_info

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
