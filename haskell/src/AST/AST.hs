module AST.AST (
    Node(..),
    Value(..),
    print_node,
    debug_print_node,
    Environment,
    print_env
) where

import qualified Data.Map as Map

import qualified Lexer.Tokenize as Tokenize

type Environment = Map.Map String Node

print_env :: Environment -> String
print_env env =
    "Environment:\n" ++ print_all_entries (Map.toList env) "\n"

print_entry :: String -> Node -> String
print_entry key val = key ++ " -> " ++ (print_node val)

print_all_entries :: [(String, Node)] -> String -> String
print_all_entries [] _ = ""
print_all_entries ((key, val): env) delimiter =
    (print_entry key val) ++ delimiter ++ (print_all_entries env delimiter)

data Value
    = IntNode Int
    | BoolNode Bool
    | NullNode
    | VariableNode String
    | IfNode Node Node Node
    | LambdaNode Node [String]
    | FunctionCallNode String [Node]
    | StructDeclarationNode [String]
    | StructInstantiationNode (Map.Map String Node)
    | MemberAccessNode String String
    | PrimitiveOperatorNode ([Node] -> Node)
    | PrimitiveUnaryOperatorNode (Node -> Node)
    | ClosureNode Node [String] Environment
    | ModuleDefinitionNode
    | EnvContainerNode Environment

data Node = Node Value Tokenize.TokenLoc

debug_print_node :: Node -> String
debug_print_node (Node node file_loc_info) =
    (value node debug_print_node) ++ " at " ++ (show file_loc_info)

print_node :: Node -> String
print_node (Node node _) =
    value node print_node

value :: Value -> (Node -> String) -> String
value val printer =
    case val of
        (IntNode int) -> show int
        (BoolNode bool) -> show bool
        (NullNode) -> "null"
        (VariableNode name) ->
            "(variable " ++ name ++ ")"
        (IfNode if_expr then_expr else_expr) ->
            "(if " ++ (printer if_expr) ++ " then " ++ (printer then_expr) ++ " else " ++ (printer else_expr) ++ ")"
        (LambdaNode body operands) ->
            "(lambda " ++ show operands ++ (printer body) ++ ")"
        (FunctionCallNode name arguments) ->
            "(" ++ name ++ (show (map printer arguments)) ++ ")"
        (StructDeclarationNode fields) ->
            "(struct " ++ show fields ++ ")"
        (MemberAccessNode struct_name member_name) ->
            "(member " ++ struct_name ++ " " ++ member_name ++ ")"
        (StructInstantiationNode field_mapping) ->
            "(instance " ++ print_all_entries (Map.toList field_mapping) " " ++ " )"
        (ClosureNode body arguments _) ->
            "(closure " ++ (printer body) ++ (show arguments) ++ "env)"
        (PrimitiveOperatorNode _) ->
            "(primitive_operator [])"
        (PrimitiveUnaryOperatorNode _) ->
            "(primitive_unary_operator _)"
        (ModuleDefinitionNode) ->
            "(module)"
        (EnvContainerNode _) ->
            "(env_container )"
