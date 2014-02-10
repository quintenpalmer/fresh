module AST.Env (
    defaultEnvironment
) where

import qualified Data.Map as Map

import qualified Lexer.Tokenize as Tokenize

import qualified AST.AST as AST
import qualified AST.Primitive as Primitive

primitive_container_builder :: Primitive.InfiniteOperator -> AST.NodeContainer
primitive_container_builder node =
    AST.NodeContainer
        (AST.PrimitiveOperatorNode node)
        (Tokenize.FileLocInfo
            (Tokenize.FileLoc 0 0)
            (Tokenize.FileLoc 0 0)
        )

defaultEnvironment :: AST.Environment
defaultEnvironment = Map.fromList [
    ("+", primitive_container_builder Primitive.add),
    ("-", primitive_container_builder Primitive.minus),
    ("*", primitive_container_builder Primitive.multiply),
    ("and", primitive_container_builder Primitive.and_all),
    ("or", primitive_container_builder Primitive.or_all),
    (">", primitive_container_builder Primitive.greater),
    ("<", primitive_container_builder Primitive.less),
    ("=", primitive_container_builder Primitive.equal)]
