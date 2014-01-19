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
evaluate_with_env (AST.BindingNode name expression body) env =
    evaluate_with_env body (Map.insert name (evaluate_with_env expression env) env)
evaluate_with_env (AST.VariableNode name) env =
    let maybe_variable = Map.lookup name env
    in
        if Maybe.isJust maybe_variable then
            Maybe.fromJust maybe_variable
        else
            error $ "Variable '" ++ name ++ "' not found"
