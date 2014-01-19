module Evaluation (
    evaluate
) where

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import qualified AST
import qualified Runtime as RT

evaluate :: AST.Node -> RT.Environment -> RT.RuntimeType
evaluate node env =
    evaluate_with_env node env

evaluate_with_env :: AST.Node -> RT.Environment -> RT.RuntimeType
evaluate_with_env (AST.IntNode value) env = RT.IntType value
evaluate_with_env (AST.BoolNode value) env = RT.BoolType value
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
