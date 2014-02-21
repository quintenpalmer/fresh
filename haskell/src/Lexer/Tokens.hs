module Lexer.Tokens (
    Token(..),
    TokenType(..),
    print_token,
    print_tokens
) where

import qualified Tools.Formatting as Formatting
import qualified Lexer.SourceInfo as SourceInfo

data Token = Token TokenType String SourceInfo.TokenLoc deriving (Show)

data TokenType
    = IntLiteral
    | BoolLiteral
    | NullLiteral
    | IfLiteral
    | LambdaLiteral
    | MemberLiteral
    | VarLiteral
    | FunctionLiteral
    | TypeLiteral
    | StructLiteral
    | String_
    | Eof
    | LParen
    | RParen
    | LBracket
    | RBracket deriving (Show, Eq)

print_tokens :: [Token] -> String
print_tokens tokens =
    let folder current rest = current ++ "\n" ++ rest
    in
        foldr folder " " (map print_token tokens)

print_token :: Token -> String
print_token (Token token_type string file_info) =
    "token_type " ++ Formatting.postfix_spaces (show token_type) 10 ++
    " " ++  Formatting.postfix_spaces string 7 ++
    SourceInfo.print_file_info file_info
