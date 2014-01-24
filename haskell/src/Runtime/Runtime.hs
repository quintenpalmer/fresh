module Runtime.Runtime (
    Environment,
    value,
    RuntimeType(..)
) where

import qualified Data.Map as Map

import qualified Parser.AST as AST

type Environment = Map.Map String RuntimeType

data RuntimeType
    = IntType Int
    | BoolType Bool
    | ClosureType AST.Node [String] Environment
    | BuiltinClosureType ([RuntimeType] -> RuntimeType)
    | StructDeclarationType [String]
    | StructInstantiationType (Map.Map String RuntimeType)

value :: RuntimeType -> String
value (IntType i) = show i
value (BoolType b) = show b
value (ClosureType _ args _) =
    "Closure with args " ++ foldl (\ first second -> first ++ " "++ second) ""args
value (BuiltinClosureType _) = "Builtin Closure"
value (StructDeclarationType args) =
    "Struct with" ++
    foldl (\ first second -> first ++ " " ++ second) "" args
value (StructInstantiationType _) =
    "Object with"
