module Runtime (
    Environment,
    value,
    RuntimeType(..)
) where

import qualified Data.Map as Map

import qualified AST

type Environment = Map.Map String RuntimeType

data RuntimeType
    = IntType Int
    | BoolType Bool
    | ClosureType AST.Node [String] Environment
    | BuiltinClosureType ([RuntimeType] -> RuntimeType)

value :: RuntimeType -> String
value (IntType i) = show i
value (BoolType b) = show b
value (ClosureType _ args _) =
    "Closure with args " ++ foldl (\ first second -> first ++ " "++ second) ""args
value (BuiltinClosureType _) = "Builtin Closure"
