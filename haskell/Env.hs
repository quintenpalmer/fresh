module Env(
    defaultEnvironment
) where

import qualified Data.Map as Map

import qualified Builtin
import qualified Runtime

defaultEnvironment :: Runtime.Environment
defaultEnvironment = Map.fromList [
    ("+", Runtime.BuiltinClosureType(Builtin.add)),
    ("-", Runtime.BuiltinClosureType(Builtin.minus)),
	("*", Runtime.BuiltinClosureType(Builtin.multiply)),
	("and", Runtime.BuiltinClosureType(Builtin.and_all)),
	("or", Runtime.BuiltinClosureType(Builtin.or_all)),
	(">", Runtime.BuiltinClosureType(Builtin.greater)),
	("<", Runtime.BuiltinClosureType(Builtin.less)),
	("=", Runtime.BuiltinClosureType(Builtin.equal))]
