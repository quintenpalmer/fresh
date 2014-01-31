module Tools.Formatting (
    prefix_spaces,
    postfix_spaces
) where

postfix_spaces :: String -> Int -> String
postfix_spaces string len =
    postfix_string string (len - (length string)) " "

prefix_spaces :: String -> Int -> String
prefix_spaces string len =
    prefix_string string (len - (length string)) " "

prefix_string :: String -> Int -> String -> String
prefix_string string len prefix =
    if len < 0 then
        string
    else
        prefix_string (prefix ++ string) (len - 1) prefix

postfix_string :: String -> Int -> String -> String
postfix_string string len postfix =
    if len < 0 then
        string
    else
        postfix_string (string ++ postfix) (len - 1) postfix
