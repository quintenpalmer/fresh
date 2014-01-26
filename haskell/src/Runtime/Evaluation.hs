module Runtime.Evaluation (
    evaluate
) where

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import qualified Parser.AST as AST

evaluate :: AST.Node -> AST.Environment -> AST.Node
evaluate (AST.IntNode value) _ = AST.IntNode value
evaluate (AST.BoolNode value) _ = AST.BoolNode value
evaluate (AST.IfNode cond_expr then_expr else_expr) env =
    case evaluate cond_expr env of
        (AST.BoolNode True) -> evaluate then_expr env
        (AST.BoolNode False) -> evaluate else_expr env
        _ -> error "if expression must be boolean"
evaluate (AST.BindingNode name expression body) env =
    evaluate body (Map.insert name (evaluate expression env) env)
evaluate (AST.VariableNode name) env =
    let maybe_variable = Map.lookup name env
    in
        if Maybe.isJust maybe_variable then
            Maybe.fromJust maybe_variable
        else
            error $ "Variable '" ++ name ++ "' not found"
evaluate (AST.LambdaNode body arguments) env =
    AST.ClosureNode body arguments env
evaluate (AST.FunctionCallNode name values) env =
    let maybe_function = Map.lookup name env
    in
        if Maybe.isJust maybe_function then
            evaluate_function_call (Maybe.fromJust maybe_function) values env
        else
            error $ "Function '" ++ name ++ "' not in scope"
evaluate (AST.StructDeclarationNode members) _ =
    AST.StructDeclarationNode members
evaluate (AST.StructInstantiationNode fields) _ =
    AST.StructInstantiationNode fields
evaluate (AST.BuiltinNode builtin_function) _ =
    AST.BuiltinNode builtin_function
evaluate (AST.ClosureNode body arguments env) _ =
    AST.ClosureNode body arguments env
evaluate (AST.MemberAccessNode struct_name member_name) env =
    let maybe_struct = Map.lookup struct_name env
    in
        if Maybe.isJust maybe_struct then
            let actual_struct = (Maybe.fromJust maybe_struct)
            in
                case actual_struct of
                    (AST.StructInstantiationNode fields) ->
                        let maybe_value = Map.lookup member_name fields
                        in
                            if Maybe.isJust maybe_value then
                                Maybe.fromJust maybe_value
                            else
                                error $ "Struct '" ++ struct_name ++ "' has no field '" ++ member_name
                    _ -> error "Not an object in scope"
        else
            error $ "Struct '" ++ struct_name ++ "' not in scope"


evaluate_function_call :: AST.Node -> [AST.Node] -> AST.Environment -> AST.Node
evaluate_function_call (AST.BuiltinNode function) values env =
    function $ map (flip evaluate env) values
evaluate_function_call (AST.ClosureNode function arguments closure_env) values env =
    evaluate function $ build_new_env
        arguments
        (map (flip evaluate env) values)
        $ Map.union closure_env env
evaluate_function_call (AST.StructDeclarationNode arguments) values env =
    AST.StructInstantiationNode $ build_new_env
        arguments
        (map (flip evaluate env) values)
        (Map.fromList [])


evaluate_function_call _ _ _ =
    error "Invalid runtime type, expected function call closure"

build_new_env :: [String] -> [AST.Node] -> AST.Environment -> AST.Environment
build_new_env [] [] env = env
build_new_env (argument:arguments) (value:values) env =
    build_new_env arguments values $ Map.insert argument value env
build_new_env _ _ _ =
    error "Mismatched number of arguments"
