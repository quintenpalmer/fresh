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
