module Parser.Chomper (
    parse_fields,
    parse_params,
    parse_name,
    chomp_close_expression,
    chomp_close_lambda_params,
    chomp_open_lambda_params,
    chomp_open_expression
) where

import qualified Lexer.Tokenize as Tok

type Token = Tok.Token

parse_fields :: [String] -> [Token] -> ([String], [Token])
parse_fields _ [] = error "No fields to parse in parse_fields"
parse_fields existing_params input_tokens@((Tok.Token token_type name _):tokens) =
    case token_type of
        Tok.RParen -> (existing_params, input_tokens)
        Tok.String_ -> parse_fields (name: existing_params) tokens
        _ -> error $ "Was expecting parameter or ) when found " ++ name

parse_params :: [String] -> [Token] -> ([String], [Token])
parse_params _ [] = error "No parameters to parse in parse_params"
parse_params existing_params input_tokens@((Tok.Token token_type name _):tokens) =
    case token_type of
        Tok.RBracket -> (existing_params, input_tokens)
        Tok.String_ -> parse_params (name: existing_params) tokens
        _ -> error $ "Was expecting parameter or ] when found " ++ name

parse_name :: [Token] -> (String, [Token])
parse_name [] = error "No parameters to parse in parse_params"
parse_name ((Tok.Token token_type name _):tokens) =
    case token_type of
        Tok.String_ -> (name, tokens)
        _ -> error $ "Was expecting name, found " ++ name

chomp_open_expression :: [Token] -> [Token]
chomp_open_expression tokens =
    assert_chomping Tok.LParen tokens

chomp_close_expression :: [Token] -> [Token]
chomp_close_expression tokens =
    assert_chomping Tok.RParen tokens

chomp_open_lambda_params :: [Token] -> [Token]
chomp_open_lambda_params tokens =
    assert_chomping Tok.LBracket tokens

chomp_close_lambda_params :: [Token] -> [Token]
chomp_close_lambda_params tokens =
    assert_chomping Tok.RBracket tokens

assert_chomping :: Tok.TokenType -> [Token] -> [Token]
assert_chomping expected [] = error $ "End of tokens when asserting for " ++ show expected
assert_chomping expected_token_type ((Tok.Token token_type string file_info):tokens) =
    if token_type == expected_token_type then
        tokens
    else
        error $ "wrong token'" ++ (show expected_token_type) ++ "', found '" ++ string ++ "'" ++ show file_info
