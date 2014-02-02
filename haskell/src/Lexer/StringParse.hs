module Lexer.StringParse (
    get_next_non_whitespace,
    get_next_character,
) where

import qualified Lexer.Chars as Chars
import qualified Lexer.FileLoc as FileLoc

type FileLocInfo = FileLoc.FileLocInfo

get_next_non_whitespace :: String -> FileLocInfo -> (Char, String, FileLocInfo)
get_next_non_whitespace remaining input_info =
    let (current, post_remaining, info) = get_next_character remaining input_info
    in
        if Chars.is_whitespace current then
            get_next_non_whitespace post_remaining info
        else
            (current, post_remaining, info)

get_next_character :: String -> FileLocInfo -> (Char, String, FileLocInfo)
get_next_character [] _ = error "Reading past end of file"
get_next_character [current] info = (current, [Chars.eof], info)
get_next_character (current: rest) (FileLoc.FileLocInfo (FileLoc.FileLoc start_char start_line) (FileLoc.FileLoc end_char end_line)) =
    if Chars.is_line_break current then
        (current, rest, (FileLoc.FileLocInfo (FileLoc.FileLoc 1 start_line) (FileLoc.FileLoc 1 (end_line + 1))))
    else
        (current, rest, (FileLoc.FileLocInfo (FileLoc.FileLoc start_char start_line) (FileLoc.FileLoc (end_char + 1) end_line)))
