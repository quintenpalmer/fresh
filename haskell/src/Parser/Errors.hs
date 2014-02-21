module Parser.Errors (
    unexpected_eof
) where

unexpected_eof :: String -> String
unexpected_eof name = "Unexpected end of file while parsing " ++ name

