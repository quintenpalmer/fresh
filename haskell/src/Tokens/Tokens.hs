module Tokens.Tokens (
    Token(..),
    TokenType(..),
    FileLoc(..),
    TokenLoc(..),
    print_tokens
) where

import qualified Debug.Debug as Debug
import qualified Tools.Formatting as Formatting

data FileLoc = FileLoc Int Int

data TokenLoc = TokenLoc FileLoc FileLoc

instance Debug.DebugShow TokenLoc where
    debug_show (TokenLoc (FileLoc start_line start_char) (FileLoc end_line end_char)) =
        "line: " ++ Formatting.prefix_spaces (show start_line) 3 ++
        " char: " ++ Formatting.prefix_spaces (show start_char) 3 ++
        " to line: " ++ Formatting.prefix_spaces (show end_line) 3 ++
        " char: " ++ Formatting.prefix_spaces (show end_char) 3

instance Show TokenLoc where
    show (TokenLoc (FileLoc start_line start_char) (FileLoc end_line end_char)) =
        "(" ++ (show start_line) ++
        ":" ++ (show start_char) ++
        ") - (" ++ (show end_line) ++
        ":" ++ (show end_char) ++
        ")"

data Token = Token TokenType String TokenLoc

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
    | Type
    | Colon
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
