module Runtime.Evaluation (
    start_evaluate,
    create_evaluated_env
) where

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import qualified AST.AST as AST

start_evaluate :: AST.NodeContainer -> AST.Environment -> AST.NodeContainer
start_evaluate node env =
    evaluate node $ create_evaluated_env env

create_evaluated_env :: AST.Environment -> AST.Environment
create_evaluated_env env =
    evaluate_environment env Map.empty

evaluate_environment :: AST.Environment -> AST.Environment -> AST.Environment
evaluate_environment pre_eval post_eval =
    if Map.null pre_eval then
        post_eval
    else
        let ((key, val):rest) = Map.toList pre_eval
        in
            evaluate_environment (Map.fromList rest) (Map.insert key (evaluate val post_eval) post_eval)


evaluate :: AST.NodeContainer -> AST.Environment -> AST.NodeContainer
evaluate node@(AST.NodeContainer (AST.IntNode _) _) _ = node
evaluate node@(AST.NodeContainer (AST.BoolNode _) _) _ = node
evaluate node@(AST.NodeContainer (AST.NullNode) _) _ = node
evaluate (AST.NodeContainer (AST.IfNode cond_expr then_expr else_expr) file_info) env =
    case evaluate cond_expr env of
        (AST.NodeContainer (AST.BoolNode True) _) -> evaluate then_expr env
        (AST.NodeContainer (AST.BoolNode False) _) -> evaluate else_expr env
        _ -> error $ "if expression must be boolean at " ++ show file_info
evaluate (AST.NodeContainer (AST.VariableNode name) file_info) env =
    let maybe_variable = Map.lookup name env
    in
        if Maybe.isJust maybe_variable then
            Maybe.fromJust maybe_variable
        else
            error $ "Variable '" ++ name ++ "' not found at " ++ show file_info
evaluate (AST.NodeContainer (AST.LambdaNode body arguments) file_loc) env =
    AST.NodeContainer (AST.ClosureNode body arguments env) file_loc
evaluate (AST.NodeContainer (AST.FunctionCallNode name values) file_info) env =
    let maybe_function = Map.lookup name env
    in
        if Maybe.isJust maybe_function then
            evaluate_function_call (Maybe.fromJust maybe_function) values env
        else
            error $ "Function '" ++ name ++ "' not in scope at " ++ show file_info
evaluate node@(AST.NodeContainer (AST.StructDeclarationNode _) _) _ = node
evaluate node@(AST.NodeContainer (AST.StructInstantiationNode _) _) _ = node
evaluate node@(AST.NodeContainer (AST.PrimitiveOperatorNode _) _) _ = node
evaluate node@(AST.NodeContainer (AST.PrimitiveUnaryOperatorNode _) _) _ = node
evaluate node@(AST.NodeContainer (AST.ClosureNode _ _ _) _) _ = node
evaluate (AST.NodeContainer (AST.MemberAccessNode struct_name member_name) file_info) env =
    let maybe_struct = Map.lookup struct_name env
    in
        if Maybe.isJust maybe_struct then
            let actual_struct = (Maybe.fromJust maybe_struct)
            in
                case actual_struct of
                    (AST.NodeContainer (AST.StructInstantiationNode fields) inner_file_info) ->
                        let maybe_value = Map.lookup member_name fields
                        in
                            if Maybe.isJust maybe_value then
                                Maybe.fromJust maybe_value
                            else
                                error $ "Struct '" ++ struct_name ++ "' has no field '" ++ member_name ++ " at " ++ show inner_file_info
                    _ -> error "Not an object in scope"
        else
            error $ "Struct '" ++ struct_name ++ "' not in scope at " ++ show file_info
evaluate (AST.NodeContainer (AST.ModuleDefinitionNode) file_info) env =
    (AST.NodeContainer (AST.EnvContainerNode env) file_info)
evaluate node@(AST.NodeContainer (AST.EnvContainerNode _) _) _ = node


evaluate_function_call :: AST.NodeContainer -> [AST.NodeContainer] -> AST.Environment -> AST.NodeContainer
evaluate_function_call (AST.NodeContainer (AST.PrimitiveOperatorNode function) _) values env =
    function $ map (flip evaluate env) values
evaluate_function_call (AST.NodeContainer (AST.PrimitiveUnaryOperatorNode function) _) values env =
    function (evaluate (head values) env)
evaluate_function_call (AST.NodeContainer (AST.ClosureNode function arguments closure_env) _) values env =
    evaluate function $ build_new_env
        arguments
        (map (flip evaluate env) values)
        (Map.union closure_env env)
evaluate_function_call (AST.NodeContainer (AST.StructDeclarationNode arguments) file_info) values env =
    AST.NodeContainer
        (AST.StructInstantiationNode $ build_new_env
            arguments
            (map (flip evaluate env) values)
            (Map.fromList []))
        file_info

evaluate_function_call node _ _ =
    error $ "Invalid runtime type, expected function call closure: got " ++ AST.print_node node

build_new_env :: [String] -> [AST.NodeContainer] -> AST.Environment -> AST.Environment
build_new_env [] [] env = env
build_new_env (argument:arguments) (value:values) env =
    build_new_env arguments values $ Map.insert argument value env
build_new_env _ _ _ =
    error "Mismatched number of arguments"
