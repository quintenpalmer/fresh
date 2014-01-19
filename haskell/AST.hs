module AST (
    Node(..)
) where

data Node
    = IntNode Int
    | BoolNode Bool deriving (Show)
