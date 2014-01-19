module Runtime (
    Environment,
    RuntimeType(..)
) where

import qualified Data.Map as Map

import qualified AST

type Environment = Map.Map String RuntimeType

data RuntimeType
    = IntType {int :: Int}
    | BoolType {bool :: Bool}
