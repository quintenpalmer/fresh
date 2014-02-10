module AST.AST (
    NodeContainer(..),
    Node(..),
    print_node,
    debug_print_node,
    Environment,
    print_env
) where

import qualified Data.Map as Map

import qualified Lexer.Tokenize as Tokenize

type Environment = Map.Map String NodeContainer

print_env :: Environment -> String
print_env env =
    "Environment:\n" ++ print_all_entries (Map.toList env) "\n"

print_entry :: String -> NodeContainer -> String
print_entry key val = key ++ " -> " ++ (print_node val)

print_all_entries :: [(String, NodeContainer)] -> String -> String
print_all_entries [] _ = ""
print_all_entries ((key, val): env) delimiter =
    (print_entry key val) ++ delimiter ++ (print_all_entries env delimiter)

print_struct_fields :: Environment -> String
print_struct_fields env =
    print_all_entries (Map.toList env) " "

data Node
    = IntNode Int
    | BoolNode Bool
    | VariableNode String
    | IfNode NodeContainer NodeContainer NodeContainer
    | LambdaNode NodeContainer [String]
    | FunctionCallNode String [NodeContainer]
    | StructDeclarationNode [String]
    | StructInstantiationNode (Map.Map String NodeContainer)
    | MemberAccessNode String String
    | PrimitiveOperatorNode ([NodeContainer] -> NodeContainer)
    | ClosureNode NodeContainer [String] Environment

data NodeContainer = NodeContainer Node Tokenize.FileLocInfo

debug_print_node :: NodeContainer -> String
debug_print_node (NodeContainer node file_loc_info) =
    (value node debug_print_node) ++ " at " ++ (Tokenize.print_file_info file_loc_info)

print_node :: NodeContainer -> String
print_node (NodeContainer node _) =
    value node print_node

value :: Node -> (NodeContainer -> String) -> String
value node printer =
    case node of
        (IntNode int) -> show int
        (BoolNode bool) -> show bool
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
            "(instance " ++ print_struct_fields field_mapping ++ " )"
        (ClosureNode body arguments _) ->
            "(closure " ++ (printer body) ++ (show arguments) ++ "env)"
        (PrimitiveOperatorNode _) ->
            "(primitive_operator [])"
