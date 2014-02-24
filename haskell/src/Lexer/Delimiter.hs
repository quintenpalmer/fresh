module Lexer.Delimiter (
    get_token_string,
    is_whitespace,
) where

import qualified Lexer.SourceInfo as SourceInfo

type TokenLoc = SourceInfo.TokenLoc

line_breaks :: [Char]
line_breaks = ['\n']

whitespace :: [Char]
whitespace = [' ', '\r', '\t'] ++ line_breaks

delimiters :: [Char]
delimiters = ['(', ')', '[', ']', ':'] ++ whitespace

is_line_break :: Char -> Bool
is_line_break char =
    char `elem` line_breaks

is_whitespace :: Char -> Bool
is_whitespace char =
    char `elem` whitespace

is_delimiter :: Char -> Bool
is_delimiter char =
    char `elem` delimiters

get_token_string :: String -> TokenLoc -> (String, String, TokenLoc)
get_token_string remaining (SourceInfo.TokenLoc _ end_file_loc_info) =
    let (current, post_remaining, file_info) = get_next_non_whitespace remaining (SourceInfo.TokenLoc end_file_loc_info end_file_loc_info)
    in
        if is_delimiter current then
            ([current], post_remaining, file_info)
        else
            build_next_token [current] post_remaining file_info

build_next_token :: String -> String -> TokenLoc -> (String, String, TokenLoc)
build_next_token input_token remaining input_file_info =
    let (current, post_remaining, file_info) = get_next_character remaining input_file_info
    in
        if is_delimiter current then
            (input_token, remaining, file_info)
        else
            build_next_token (input_token ++ [current]) post_remaining file_info

get_next_non_whitespace :: String -> TokenLoc -> (Char, String, TokenLoc)
get_next_non_whitespace remaining input_info =
    let (current, post_remaining, info) = get_next_character remaining input_info
    in
        if is_whitespace current then
            get_next_non_whitespace post_remaining info
        else
            (current, post_remaining, info)

get_next_character :: String -> TokenLoc -> (Char, String, TokenLoc)
get_next_character [] _ = error "Reading past end of file"
get_next_character [current] info = (current, [], info)
get_next_character (current: rest) (SourceInfo.TokenLoc (SourceInfo.FileLoc start_line start_char) (SourceInfo.FileLoc end_line end_char)) =
    if is_line_break current then
        (current, rest, (SourceInfo.TokenLoc (SourceInfo.FileLoc start_line 1) (SourceInfo.FileLoc (end_line + 1) 1)))
    else
        (current, rest, (SourceInfo.TokenLoc (SourceInfo.FileLoc start_line start_char) (SourceInfo.FileLoc end_line (end_char + 1))))
