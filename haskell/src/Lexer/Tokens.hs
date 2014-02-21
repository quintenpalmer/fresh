module Lexer.Tokens (
    Token(..),
    TokenType(..),
    print_tokens
) where

import qualified Debug.Debug as Debug
import qualified Tools.Formatting as Formatting
import qualified Lexer.SourceInfo as SourceInfo

data Token = Token TokenType String SourceInfo.TokenLoc

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
    | PackageLiteral
    | String_
    | LParen
    | RParen
    | LBracket
    | RBracket deriving (Show, Eq)

print_tokens :: [Token] -> String
print_tokens tokens =
    case tokens of
        [] -> ""
        (current: rest) -> (Debug.debug_show current) ++ "\n" ++ print_tokens rest

instance Debug.DebugShow Token where
    debug_show (Token token_type string file_info) =
        Formatting.postfix_spaces string 15 ++
        " (" ++ Formatting.postfix_spaces (show token_type ++ ")") 20 ++
        " at " ++ Debug.debug_show file_info

instance Show Token where
    show (Token token_type string file_info) =
        Formatting.postfix_spaces string 15 ++
        " (" ++ Formatting.postfix_spaces (show token_type ++ ")") 20 ++
        " at " ++ show file_info
