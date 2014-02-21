module AST.BuiltinEnv (
    defaultEnvironment
) where

import qualified Data.Map as Map

import qualified Lexer.Tokenize as Tokenize

import qualified AST.AST as AST
import qualified AST.Primitive as Primitive

infinite_operand_node_builder :: Primitive.InfiniteOperator -> AST.Node
infinite_operand_node_builder node =
    AST.Node
        (AST.PrimitiveOperatorNode node)
        (Tokenize.TokenLoc (Tokenize.FileLoc 0 0) (Tokenize.FileLoc 0 0))

unary_operand_node_builder :: Primitive.UnaryOperator -> AST.Node
unary_operand_node_builder node =
    AST.Node
        (AST.PrimitiveUnaryOperatorNode node)
        (Tokenize.TokenLoc (Tokenize.FileLoc 0 0) (Tokenize.FileLoc 0 0))

defaultEnvironment :: AST.Environment
defaultEnvironment = Map.fromList [
    ("+", infinite_operand_node_builder Primitive.add),
    ("-", infinite_operand_node_builder Primitive.minus),
    ("*", infinite_operand_node_builder Primitive.multiply),
    ("and", infinite_operand_node_builder Primitive.and_all),
    ("or", infinite_operand_node_builder Primitive.or_all),
    (">", infinite_operand_node_builder Primitive.greater),
    ("<", infinite_operand_node_builder Primitive.less),
    ("=", infinite_operand_node_builder Primitive.equal),
    ("is_null", unary_operand_node_builder Primitive.is_null)]
