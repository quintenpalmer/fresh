module Lexer.Tokenize (
    make_tokens,
    SourceInfo.FileLoc(..),
    SourceInfo.TokenLoc(..),
    Tokens.Token(..),
    Tokens.TokenType(..),
    Tokens.print_tokens
) where

import qualified Lexer.Delimiter as Delimiter
import qualified Lexer.SourceInfo as SourceInfo
import qualified Lexer.Literals as Literals
import qualified Lexer.Tokens as Tokens

type Token = Tokens.Token
type TokenType = Tokens.TokenType
type TokenLoc = SourceInfo.TokenLoc

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
            (Tokens.Token (get_token_type token) token file_info): to_tokens remaining file_info

get_token_type :: String -> TokenType
get_token_type current
    | Literals.is_open_expression current = Tokens.LParen
    | Literals.is_close_expression current = Tokens.RParen
    | Literals.is_open_arguments current = Tokens.LBracket
    | Literals.is_close_arguments current = Tokens.RBracket
    | Literals.is_int_literal current = Tokens.IntLiteral
    | Literals.is_bool_literal current   = Tokens.BoolLiteral
    | Literals.is_null_literal current = Tokens.NullLiteral
    | Literals.is_lambda_literal current = Tokens.LambdaLiteral
    | Literals.is_if_literal current = Tokens.IfLiteral
    | Literals.is_var_literal current = Tokens.VarLiteral
    | Literals.is_function_literal current = Tokens.FunctionLiteral
    | Literals.is_type_literal current = Tokens.TypeLiteral
    | Literals.is_struct_literal current = Tokens.StructLiteral
    | Literals.is_member_literal current = Tokens.MemberLiteral
    | Literals.is_package_literal current = Tokens.PackageLiteral
    | Delimiter.is_eof current = Tokens.Eof
    | otherwise = Tokens.String_
