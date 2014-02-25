module Lexer.Lexer (
    make_tokens,
) where

import qualified Tokens.Tokens as Tokens

import qualified Lexer.Delimiter as Delimiter
import qualified Lexer.Literals as Literals

type Token = Tokens.Token
type TokenType = Tokens.TokenType
type TokenLoc = Tokens.TokenLoc

make_tokens :: String -> [Token]
make_tokens input_string =
    to_tokens input_string (Tokens.TokenLoc (Tokens.FileLoc 1 0) (Tokens.FileLoc 1 0))

to_tokens :: String -> TokenLoc -> [Token]
to_tokens input_string input_file_info =
    if all Delimiter.is_whitespace input_string then
        []
    else
        let (token, remaining, file_info) = Delimiter.get_token_string input_string input_file_info
        in
            (Tokens.Token (get_token_type token) token file_info): to_tokens remaining file_info

get_token_type :: String -> TokenType
get_token_type current
    | Literals.is_type_delimiter current = Tokens.Colon
    | Literals.is_type current = Tokens.Type
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
    | otherwise = Tokens.String_
