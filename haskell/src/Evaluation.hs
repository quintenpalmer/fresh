module Evaluation (
    evaluate
) where

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import qualified AST
import qualified Runtime as RT
import qualified Util

evaluate :: AST.Node -> RT.Environment -> RT.RuntimeType
evaluate node env =
    evaluate_with_env node env

evaluate_with_env :: AST.Node -> RT.Environment -> RT.RuntimeType
evaluate_with_env (AST.IntNode value) _ = RT.IntType value
evaluate_with_env (AST.BoolNode value) _ = RT.BoolType value
evaluate_with_env (AST.IfNode cond_expr then_expr else_expr) env =
    if RT.bool $ evaluate_with_env cond_expr env then
        evaluate_with_env then_expr env
    else
        evaluate_with_env else_expr env
evaluate_with_env (AST.BindingNode name expression body) env =
    evaluate_with_env body (Map.insert name (evaluate_with_env expression env) env)
evaluate_with_env (AST.VariableNode name) env =
    let maybe_variable = Map.lookup name env
    in
        if Maybe.isJust maybe_variable then
            Maybe.fromJust maybe_variable
        else
            error $ "Variable '" ++ name ++ "' not found"
evaluate_with_env (AST.LambdaNode body arguments) env =
    RT.ClosureType body arguments env
evaluate_with_env (AST.FunctionCallNode name values) env =
    let maybe_function = Map.lookup name env
    in
        if Maybe.isJust maybe_function then
            evaluate_function_call (Maybe.fromJust maybe_function) values env
        else
            error $ "Function '" ++ name ++ "' not in scope"


evaluate_function_call :: RT.RuntimeType -> [AST.Node] -> RT.Environment -> RT.RuntimeType
evaluate_function_call (RT.BuiltinClosureType function) values env =
    function $ Util.map_many_on_one (map evaluate_with_env values) env

evaluate_function_call (RT.ClosureType function arguments closure_env) values env =
    evaluate_with_env function $ build_new_env
        arguments
        (Util.map_many_on_one (map evaluate_with_env values) env)
        $ Map.union closure_env env

evaluate_function_call _ _ _ =
    error "Invalid runtime type, expected function call closure"

build_new_env :: [String] -> [RT.RuntimeType] -> RT.Environment -> RT.Environment
build_new_env [] [] env = env
build_new_env [argument] [value] env = Map.insert argument value env
build_new_env (argument:arguments) (value:values) env =
    build_new_env arguments values
        $ Map.insert argument value env
build_new_env _ _ _ =
    error "Mismatched number of arguments"
