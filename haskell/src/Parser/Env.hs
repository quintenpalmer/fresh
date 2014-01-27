module Parser.Env (
    defaultEnvironment
) where

import qualified Data.Map as Map

import qualified Lexer.Tokenize as Tokenize
import qualified Parser.AST as AST
import qualified Parser.Builtin as Builtin


defaultEnvironment :: AST.Environment
defaultEnvironment = Map.fromList [
    ("+", AST.NodeContainer (AST.BuiltinNode(Builtin.add)) (Tokenize.FileLocInfo (Tokenize.FileLoc 0 0) (Tokenize.FileLoc 0 0))),
    ("-", AST.NodeContainer (AST.BuiltinNode(Builtin.minus)) (Tokenize.FileLocInfo (Tokenize.FileLoc 0 0) (Tokenize.FileLoc 0 0))),
    ("*", AST.NodeContainer (AST.BuiltinNode(Builtin.multiply)) (Tokenize.FileLocInfo (Tokenize.FileLoc 0 0) (Tokenize.FileLoc 0 0))),
    ("and", AST.NodeContainer (AST.BuiltinNode(Builtin.and_all)) (Tokenize.FileLocInfo (Tokenize.FileLoc 0 0) (Tokenize.FileLoc 0 0))),
    ("or", AST.NodeContainer (AST.BuiltinNode(Builtin.or_all)) (Tokenize.FileLocInfo (Tokenize.FileLoc 0 0) (Tokenize.FileLoc 0 0))),
    (">", AST.NodeContainer (AST.BuiltinNode(Builtin.greater)) (Tokenize.FileLocInfo (Tokenize.FileLoc 0 0) (Tokenize.FileLoc 0 0))),
    ("<", AST.NodeContainer (AST.BuiltinNode(Builtin.less)) (Tokenize.FileLocInfo (Tokenize.FileLoc 0 0) (Tokenize.FileLoc 0 0))),
    ("=", AST.NodeContainer (AST.BuiltinNode(Builtin.equal)) (Tokenize.FileLocInfo (Tokenize.FileLoc 0 0) (Tokenize.FileLoc 0 0)))]
