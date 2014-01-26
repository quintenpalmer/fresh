module Parser.Builtin (
    add,
    minus,
    multiply,
    and_all,
    or_all,
    greater,
    less,
    equal
) where

import qualified Parser.AST as AST

type BinaryOperator = AST.Node -> AST.Node -> AST.Node
type InfiniteOperator = [AST.Node] -> AST.Node

comparison :: (Int -> Int -> Bool) -> BinaryOperator
comparison operator (AST.IntNode x) (AST.IntNode y) = AST.BoolNode $ operator x y
comparison _ _ _ = error "> < = Invalid runtime time, two ints"

arithmetic :: (Int -> Int -> Int) -> BinaryOperator
arithmetic operator (AST.IntNode x) (AST.IntNode y) = AST.IntNode $ operator x y
arithmetic _ _ _ = error "+ - * Invalid runtime time, two ints"

boolean :: (Bool -> Bool -> Bool) -> BinaryOperator
boolean operator (AST.BoolNode x) (AST.BoolNode y) = AST.BoolNode $ operator x y
boolean _ _ _ = error "and/or invalid runtime time, two bools"


acculuatively_apply :: BinaryOperator -> [AST.Node] -> AST.Node
acculuatively_apply function input_arguments =
    case input_arguments of
        [] -> error "uhhhhh, nice try! can't apply to no arguments"
        [_] -> error "uhhhhh, nice try! can't apply to one argument"
        arguments -> acc_apply_recursive function arguments

acc_apply_recursive :: BinaryOperator -> [AST.Node] -> AST.Node
acc_apply_recursive function input_arguments =
    case input_arguments of
        [] -> error "uhhhhh, nice try! can't apply to no arguments"
        [arg] -> arg
        (argument:arguments) -> function (acc_apply_recursive function arguments) argument

comparatively_apply :: BinaryOperator -> [AST.Node] -> AST.Node
comparatively_apply function input_arguments =
    case input_arguments of
        [] -> error "uhhhhh, nice try! can't apply to no arguments"
        [_] -> error "uhhhhh, nice try! can't apply to one argument"
        arguments -> comp_apply_recursive function arguments

comp_apply_recursive :: BinaryOperator -> [AST.Node] -> AST.Node
comp_apply_recursive function input_arguments =
    case input_arguments of
        [] -> error "uhhhhh, nice try! can't apply to no arguments"
        [_] -> error "uhhhhh, nice try! can't apply to one argument"
        [arg1, arg2] -> function arg2 arg1
        (arg1:(arg2:arguments)) ->
            ander
                (function arg2 arg1)
                (comp_apply_recursive function $ arg1:arguments)

adder :: BinaryOperator
adder x y = arithmetic (+) x y

add :: InfiniteOperator
add arguments = acculuatively_apply adder arguments

subtracter :: BinaryOperator
subtracter x y = arithmetic (-) x y

minus :: InfiniteOperator
minus arguments = acculuatively_apply subtracter arguments

multiplier :: BinaryOperator
multiplier x y = arithmetic (*) x y

multiply :: InfiniteOperator
multiply arguments = acculuatively_apply multiplier arguments

ander :: BinaryOperator
ander x y = boolean (&&) x y

and_all :: InfiniteOperator
and_all arguments = acculuatively_apply ander arguments

orer :: BinaryOperator
orer x y = boolean (||) x y

or_all :: InfiniteOperator
or_all arguments = acculuatively_apply orer arguments

greaterer :: BinaryOperator
greaterer x y = comparison (>) x y

greater :: InfiniteOperator
greater arguments = comparatively_apply greaterer arguments

lesser :: BinaryOperator
lesser x y = comparison (<) x y

less :: InfiniteOperator
less arguments = comparatively_apply lesser arguments

equaler :: BinaryOperator
equaler x y = comparison (==) x y

equal :: InfiniteOperator
equal arguments = comparatively_apply equaler arguments
