module AST (
    Node(..)
) where

data Node
    = IntNode Int
    | BoolNode Bool
    | VariableNode String
    | BindingNode String Node Node
    | IfNode Node Node Node
    | FunctionCallNode String [Node]
    | LambdaNode Node [String] deriving (Show)
