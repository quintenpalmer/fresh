module AST (
    Node(..)
) where

data Node
    = IntNode Int
    | BoolNode Bool
    | VariableNode String
    | BindingNode String Node Node deriving (Show)
