module Runtime.Evaluation (
    start_evaluate,
    create_evaluated_env
) where

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import qualified AST.AST as AST

start_evaluate :: AST.Environment -> AST.Node
start_evaluate env =
    let node = parse_main env
    in
        evaluate node $ create_evaluated_env env

parse_main :: AST.Environment -> AST.Node
parse_main env =
    let maybe_main = Map.lookup "main" env
    in
        if Maybe.isJust maybe_main then
            Maybe.fromJust maybe_main
        else
            error $ "main function not defined"

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


evaluate :: AST.Node -> AST.Environment -> AST.Node
evaluate (AST.Node (AST.IfNode cond_expr then_expr else_expr) file_info) env =
    case evaluate cond_expr env of
        (AST.Node (AST.BoolNode True) _) -> evaluate then_expr env
        (AST.Node (AST.BoolNode False) _) -> evaluate else_expr env
        _ -> error $ "if expression must be boolean at " ++ show file_info
evaluate (AST.Node (AST.VariableNode name) file_info) env =
    let maybe_variable = Map.lookup name env
    in
        if Maybe.isJust maybe_variable then
            Maybe.fromJust maybe_variable
        else
            error $ "Variable '" ++ name ++ "' not found at " ++ show file_info
evaluate (AST.Node (AST.LambdaNode body arguments) file_loc) env =
    AST.Node (AST.ClosureNode body arguments env) file_loc
evaluate (AST.Node (AST.FunctionCallNode name values) file_info) env =
    let maybe_function = Map.lookup name env
    in
        if Maybe.isJust maybe_function then
            evaluate_function_call (Maybe.fromJust maybe_function) values env
        else
            error $ "Function '" ++ name ++ "' not in scope at " ++ show file_info
evaluate (AST.Node (AST.MemberAccessNode struct_name member_name) file_info) env =
    let maybe_struct = Map.lookup struct_name env
    in
        if Maybe.isJust maybe_struct then
            let actual_struct = (Maybe.fromJust maybe_struct)
            in
                case actual_struct of
                    (AST.Node (AST.StructInstantiationNode fields) inner_file_info) ->
                        let maybe_value = Map.lookup member_name fields
                        in
                            if Maybe.isJust maybe_value then
                                Maybe.fromJust maybe_value
                            else
                                error $ "Struct '" ++ struct_name ++ "' has no field '" ++ member_name ++ " at " ++ show inner_file_info
                    _ -> error "Not an object in scope"
        else
            error $ "Struct '" ++ struct_name ++ "' not in scope at " ++ show file_info
evaluate (AST.Node (AST.ModuleDefinitionNode) file_info) env =
    (AST.Node (AST.EnvContainerNode env) file_info)
evaluate node@(AST.Node (AST.EnvContainerNode _) _) _ = node
evaluate node@(AST.Node (AST.IntNode _) _) _ = node
evaluate node@(AST.Node (AST.BoolNode _) _) _ = node
evaluate node@(AST.Node (AST.NullNode) _) _ = node
evaluate node@(AST.Node (AST.StructDeclarationNode _) _) _ = node
evaluate node@(AST.Node (AST.StructInstantiationNode _) _) _ = node
evaluate node@(AST.Node (AST.PrimitiveOperatorNode _) _) _ = node
evaluate node@(AST.Node (AST.PrimitiveUnaryOperatorNode _) _) _ = node
evaluate node@(AST.Node (AST.ClosureNode _ _ _) _) _ = node


evaluate_function_call :: AST.Node -> [AST.Node] -> AST.Environment -> AST.Node
evaluate_function_call (AST.Node (AST.PrimitiveOperatorNode function) _) values env =
    function $ map (flip evaluate env) values
evaluate_function_call (AST.Node (AST.PrimitiveUnaryOperatorNode function) _) values env =
    function (evaluate (head values) env)
evaluate_function_call (AST.Node (AST.ClosureNode function arguments closure_env) _) values env =
    evaluate function $ build_new_env
        arguments
        (map (flip evaluate env) values)
        (Map.union closure_env env)
evaluate_function_call (AST.Node (AST.StructDeclarationNode arguments) file_info) values env =
    AST.Node
        (AST.StructInstantiationNode $ build_new_env
            arguments
            (map (flip evaluate env) values)
            (Map.fromList []))
        file_info

evaluate_function_call node _ _ =
    error $ "Invalid runtime type, expected function call closure: got " ++ show node

build_new_env :: [String] -> [AST.Node] -> AST.Environment -> AST.Environment
build_new_env [] [] env = env
build_new_env (argument:arguments) (value:values) env =
    build_new_env arguments values $ Map.insert argument value env
build_new_env _ _ _ =
    error "Mismatched number of arguments"
