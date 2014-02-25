module Parser.Chomper (
    parse_fields,
    parse_params,
    parse_name,
    chomp_close_expression,
    chomp_close_lambda_params,
    chomp_open_lambda_params,
    chomp_open_expression
) where

import qualified Tokens.Tokens as Tokens

import qualified Parser.Errors as Errors

type Token = Tokens.Token

parse_fields :: [String] -> [Token] -> ([String], [Token])
parse_fields _ [] = error $ Errors.unexpected_eof "parsing of fields"
parse_fields existing_params input_tokens@((Tokens.Token token_type name token_loc):tokens) =
    case token_type of
        Tokens.RParen -> (existing_params, input_tokens)
        Tokens.String_ -> parse_fields (name: existing_params) tokens
        _ -> error $ "Was expecting parameter or \")\" when found \"" ++ name ++ "\" " ++ show token_loc

parse_params :: [String] -> [Token] -> ([String], [Token])
parse_params _ [] = error $ Errors.unexpected_eof "parsing of parameters"
parse_params existing_params input_tokens@((Tokens.Token token_type name token_loc):tokens) =
    case token_type of
        Tokens.RBracket -> (existing_params, input_tokens)
        Tokens.String_ -> parse_params (name: existing_params) tokens
        _ -> error $ "Was expecting parameter or \"]\" when found \"" ++ name ++ "\" " ++ show token_loc

parse_name :: [Token] -> (String, [Token])
parse_name [] = error $ Errors.unexpected_eof "parsing of name"
parse_name ((Tokens.Token token_type name _):tokens) =
    case token_type of
        Tokens.String_ -> (name, tokens)
        _ -> error $ "Was expecting name, found " ++ name

chomp_open_expression :: [Token] -> [Token]
chomp_open_expression tokens =
    assert_chomping Tokens.LParen tokens "expression"

chomp_close_expression :: [Token] -> String ->[Token]
chomp_close_expression tokens name =
    assert_chomping Tokens.RParen tokens name

chomp_open_lambda_params :: [Token] -> [Token]
chomp_open_lambda_params tokens =
    assert_chomping Tokens.LBracket tokens "lambda"

chomp_close_lambda_params :: [Token] -> [Token]
chomp_close_lambda_params tokens =
    assert_chomping Tokens.RBracket tokens "lambda"

assert_chomping :: Tokens.TokenType -> [Token] -> String -> [Token]
assert_chomping expected [] name = error $ "End of tokens when asserting for " ++ show expected ++ " in " ++ name
assert_chomping expected_token_type ((Tokens.Token token_type string file_info):tokens) name =
    if token_type == expected_token_type then
        tokens
    else
        error $ "wrong token '" ++ (show expected_token_type) ++
        "', found '" ++ string ++
        "' in expression of " ++ name ++
        " : " ++ show file_info
