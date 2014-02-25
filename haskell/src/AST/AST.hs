module AST.AST (
    Node(..),
    Value(..),
    Environment,
    print_env
) where

import qualified Data.Map as Map

import qualified Debug.Debug as Debug
import qualified Tokens.Tokens as Tokens

type Environment = Map.Map String Node

print_env :: Environment -> String
print_env env =
    "Environment:\n" ++ print_all_entries (Map.toList env) "\n"

print_all_entries :: [(String, Node)] -> String -> String
print_all_entries [] _ = ""
print_all_entries ((key, val): env) delimiter =
    (print_entry key val) ++ delimiter ++ (print_all_entries env delimiter)

print_entry :: String -> Node -> String
print_entry key val = key ++ " -> " ++ (show val)

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

data Node = Node Value Tokens.TokenLoc

instance Debug.DebugShow Node where
    debug_show (Node node file_loc_info) =
        (value node Debug.debug_show) ++ " at " ++ (show file_loc_info)

instance Show Node where
    show (Node node _) =
        value node show

show_list :: [String] -> String
show_list [] = ""
show_list (current:[]) =
    current
show_list (current:rest) =
    current ++ " " ++ show_list rest

value :: Value -> (Node -> String) -> String
value val printer =
    case val of
        (IntNode int) -> show int
        (BoolNode bool) -> show bool
        (NullNode) -> "null"
        (VariableNode name) ->
            name
        (IfNode if_expr then_expr else_expr) ->
            "(if " ++ (printer if_expr) ++
            " then " ++ (printer then_expr) ++
            " else " ++ (printer else_expr) ++ ")"
        (LambdaNode body operands) ->
            "(lambda [" ++ show_list operands ++ "] " ++ (printer body) ++ ")"
        (FunctionCallNode name arguments) ->
            "(" ++ name ++ " " ++ (show_list (map printer arguments)) ++ ")"
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
