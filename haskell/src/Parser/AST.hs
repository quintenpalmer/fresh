module Parser.AST (
    Node(..),
    value,
    Environment
) where

import qualified Data.Map as Map

type Environment = Map.Map String Node

data Node
    = IntNode Int
    | BoolNode Bool
    | VariableNode String
    | BindingNode String Node Node
    | IfNode Node Node Node
    | LambdaNode Node [String]
    | FunctionCallNode String [Node]
    | StructDeclarationNode [String]
    | StructInstantiationNode (Map.Map String Node)
    | MemberAccessNode String String
    | BuiltinNode ([Node] -> Node)
    | ClosureNode Node [String] Environment

value :: Node -> String
value (IntNode int) = show int
value (BoolNode bool) = show bool
value (VariableNode name) =
    "(variable" ++ name ++ ")"
value (BindingNode name val body) =
    "(define " ++ name ++ (value val) ++ ") in " ++ (value body)
value (IfNode if_expr then_expr else_expr) =
    "(if " ++ (value if_expr) ++ " then " ++ (value then_expr) ++ " else " ++ (value else_expr) ++ ")"
value (LambdaNode body operands) =
    "(lambda " ++ show operands ++ (value body) ++ ")"
value (FunctionCallNode name arguments) =
    "(" ++ name ++ (show (map value arguments)) ++ ")"
value (StructDeclarationNode fields) =
    "(struct " ++ show fields ++ ")"
value (MemberAccessNode struct_name member_name) =
    "(member " ++ struct_name ++ " " ++ member_name ++ ")"
value (StructInstantiationNode field_mapping) =
    "(instance " ++ (show (map (\ (name, val) -> name ++ " = " ++ (value val)) (Map.toList field_mapping))) ++ " )"
value (ClosureNode body arguments _) =
    "(closure " ++ (value body) ++ (show arguments) ++ "env)"
value (BuiltinNode _) =
    "(builtin [])"
