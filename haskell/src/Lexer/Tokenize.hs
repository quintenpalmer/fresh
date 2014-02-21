module Lexer.Tokenize (
    make_tokens,
    SourceInfo.FileLoc(..),
    SourceInfo.TokenLoc(..),
    SourceInfo.print_file_info,
    Tokens.Token(..),
    Tokens.TokenType(..),
    Tokens.print_token,
    Tokens.print_tokens
) where

import qualified Lexer.Delimiter as Delimiter
import qualified Lexer.SourceInfo as SourceInfo
import qualified Lexer.Literals as Literals
import qualified Lexer.Tokens as Tokens

type TokenLoc = SourceInfo.TokenLoc
type Token = Tokens.Token
type TokenType = Tokens.TokenType

make_tokens :: String -> [Token]
make_tokens input_string =
    to_tokens input_string (SourceInfo.TokenLoc (SourceInfo.FileLoc 1 0) (SourceInfo.FileLoc 1 0))

to_tokens :: String -> TokenLoc -> [Token]
to_tokens input_string input_file_info =
    if Delimiter.is_eof input_string then
        []
    else
        let (token, remaining, file_info) = Delimiter.get_token_string input_string input_file_info
        in
            (convert_token token file_info): to_tokens remaining file_info

convert_token :: String -> TokenLoc -> Token
convert_token current file_info =
    Tokens.Token (get_token_type current) current file_info

get_token_type :: String -> TokenType
get_token_type current
    | Literals.is_open_expression current = Tokens.LParen
    | Literals.is_close_expression current = Tokens.RParen
    | Literals.is_open_arguments current = Tokens.LBracket
    | Literals.is_close_arguments current = Tokens.RBracket
    | Delimiter.is_eof current = Tokens.Eof
    | Literals.is_int_literal current = Tokens.IntLiteral
    | Literals.is_bool_literal current   = Tokens.BoolLiteral
    | Literals.is_null_literal current = Tokens.NullLiteral
    | otherwise = Tokens.String_
