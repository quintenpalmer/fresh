module Parser.Env (
    defaultEnvironment
) where

import qualified Data.Map as Map

import qualified Parser.AST as AST
import qualified Parser.Builtin as Builtin


defaultEnvironment :: AST.Environment
defaultEnvironment = Map.fromList [
    ("+", AST.BuiltinNode(Builtin.add)),
    ("-", AST.BuiltinNode(Builtin.minus)),
    ("*", AST.BuiltinNode(Builtin.multiply)),
    ("and", AST.BuiltinNode(Builtin.and_all)),
    ("or", AST.BuiltinNode(Builtin.or_all)),
    (">", AST.BuiltinNode(Builtin.greater)),
    ("<", AST.BuiltinNode(Builtin.less)),
    ("=", AST.BuiltinNode(Builtin.equal))]
