module Runtime.Evaluation (
    evaluate
) where

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import qualified Parser.AST as AST
import qualified Runtime.Runtime as Runtime

evaluate :: AST.Node -> Runtime.Environment -> Runtime.RuntimeType
evaluate (AST.IntNode value) _ = Runtime.IntType value
evaluate (AST.BoolNode value) _ = Runtime.BoolType value
evaluate (AST.IfNode cond_expr then_expr else_expr) env =
    case evaluate cond_expr env of
        (Runtime.BoolType True) -> evaluate then_expr env
        (Runtime.BoolType False) -> evaluate else_expr env
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
    Runtime.ClosureType body arguments env
evaluate (AST.FunctionCallNode name values) env =
    let maybe_function = Map.lookup name env
    in
        if Maybe.isJust maybe_function then
            evaluate_function_call (Maybe.fromJust maybe_function) values env
        else
            error $ "Function '" ++ name ++ "' not in scope"
evaluate (AST.StructDeclarationNode members) _ =
    Runtime.StructDeclarationType members


evaluate_function_call :: Runtime.RuntimeType -> [AST.Node] -> Runtime.Environment -> Runtime.RuntimeType
evaluate_function_call (Runtime.BuiltinClosureType function) values env =
    function $ map (flip evaluate env) values
evaluate_function_call (Runtime.ClosureType function arguments closure_env) values env =
    evaluate function $ build_new_env
        arguments
        (map (flip evaluate env) values)
        $ Map.union closure_env env

evaluate_function_call _ _ _ =
    error "Invalid runtime type, expected function call closure"

build_new_env :: [String] -> [Runtime.RuntimeType] -> Runtime.Environment -> Runtime.Environment
build_new_env [] [] env = env
build_new_env (argument:arguments) (value:values) env =
    build_new_env arguments values $ Map.insert argument value env
build_new_env _ _ _ =
    error "Mismatched number of arguments"
