module Parser.Env (
    defaultEnvironment
) where

import qualified Data.Map as Map

import qualified Lexer.Tokenize as Tokenize
import qualified Parser.AST as AST
import qualified Parser.Builtin as Builtin

builtin_container_builder :: Builtin.InfiniteOperator -> AST.NodeContainer
builtin_container_builder node =
    AST.NodeContainer
        (AST.BuiltinNode node)
        (Tokenize.FileLocInfo
            (Tokenize.FileLoc 0 0)
            (Tokenize.FileLoc 0 0)
        )

defaultEnvironment :: AST.Environment
defaultEnvironment = Map.fromList [
    ("+", builtin_container_builder Builtin.add),
    ("-", builtin_container_builder Builtin.minus),
    ("*", builtin_container_builder Builtin.multiply),
    ("and", builtin_container_builder Builtin.and_all),
    ("or", builtin_container_builder Builtin.or_all),
    (">", builtin_container_builder Builtin.greater),
    ("<", builtin_container_builder Builtin.less),
    ("=", builtin_container_builder Builtin.equal)]
