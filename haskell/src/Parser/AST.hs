module Parser.AST (
    NodeContainer(..),
    Node(..),
    print_node,
    debug_print_node,
    Environment
) where

import qualified Data.Map as Map
import qualified Lexer.Tokenize as Tokenize

type Environment = Map.Map String NodeContainer

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
    | BuiltinNode ([NodeContainer] -> NodeContainer)
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
            "(variable" ++ name ++ ")"
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
            "(instance " ++ (show (map (\ (name, val) -> name ++ " -> " ++ (printer val)) (Map.toList field_mapping))) ++ " )"
        (ClosureNode body arguments _) ->
            "(closure " ++ (printer body) ++ (show arguments) ++ "env)"
        (BuiltinNode _) ->
            "(builtin [])"
