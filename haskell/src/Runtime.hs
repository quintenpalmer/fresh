module Runtime (
    Environment,
    value,
    RuntimeType(..)
) where

import qualified Data.Map as Map

import qualified AST

type Environment = Map.Map String RuntimeType

data RuntimeType
    = IntType {int :: Int}
    | BoolType {bool :: Bool}
    | ClosureType {body :: AST.Node, arguments :: [String], env :: Environment}
    | BuiltinClosureType {builtin_lambda :: ([RuntimeType] -> RuntimeType)}

value (IntType i) = show i
value (BoolType b) = show b
