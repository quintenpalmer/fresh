module Tokenize (
    to_tokens
) where

eof = '\0'
whitespace = [' ', '\n', '\r', '\t']
delimiters = ['(', ')', '[', ']', eof] ++ whitespace

to_tokens :: String -> [String]
to_tokens input_string =
    if (head input_string) == eof then
        []
    else
        let (token, remaining) = get_token input_string
        in
            token: to_tokens remaining

get_token :: String -> (String, String)
get_token remaining =
    let (current, post_remaining) = get_next_non_whitespace remaining
    in
        if current `elem` delimiters then
            ([current], post_remaining)
        else
            build_next_token [current] post_remaining

build_next_token :: String -> String -> (String, String)
build_next_token input_token remaining =
    let (current, post_remaining) = get_next_character remaining
    in
        if not $ current `elem` delimiters then
            build_next_token (input_token ++ [current]) post_remaining
        else
            (input_token, remaining)

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
