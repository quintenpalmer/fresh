module Builtin (
    add,
    minus,
    multiply,
    and_all,
    or_all,
    greater,
    less,
    equal
) where

import qualified Runtime as RT

type BinaryOperator = RT.RuntimeType -> RT.RuntimeType -> RT.RuntimeType
type InfiniteOperator = [RT.RuntimeType] -> RT.RuntimeType

comparison :: (Int -> Int -> Bool) -> BinaryOperator
comparison operator (RT.IntType x) (RT.IntType y) = RT.BoolType $ operator x y
comparison _ _ _ =
    error "Invalid runtime time, two ints"

arithmetic :: (Int -> Int -> Int) -> BinaryOperator
arithmetic operator (RT.IntType x) (RT.IntType y) = RT.IntType $ operator x y
arithmetic _ _ _ =
    error "Invalid runtime time, two ints"

boolean :: (Bool -> Bool -> Bool) -> BinaryOperator
boolean operator (RT.BoolType x) (RT.BoolType y) = RT.BoolType $ operator x y
boolean _ _ _ =
    error "Invalid runtime time, two bools"

apply_for_all :: [RT.RuntimeType] -> BinaryOperator -> Bool -> RT.RuntimeType
apply_for_all [] _ _ =
    error "uhhhhh, nice try! can't apply to no arguments"
apply_for_all [_] _ _ =
    error "uhhhhh, nice try! can't apply to one argument"
apply_for_all [arg1, arg2] operator _ =
    operator arg1 arg2
apply_for_all (argument:arguments) function accumulative =
    function argument $ apply_for_all arguments function accumulative

adder :: BinaryOperator
adder x y = arithmetic (+) x y

add :: InfiniteOperator
add arguments = apply_for_all arguments adder True

subtracter :: BinaryOperator
subtracter x y = arithmetic (-) x y

minus :: InfiniteOperator
minus arguments = apply_for_all arguments subtracter True

multiplier :: BinaryOperator
multiplier x y = arithmetic (*) x y

multiply :: InfiniteOperator
multiply arguments = apply_for_all arguments multiplier True

ander :: BinaryOperator
ander x y = boolean (&&) x y

and_all :: InfiniteOperator
and_all arguments = apply_for_all arguments ander True

orer :: BinaryOperator
orer x y = boolean (||) x y

or_all :: InfiniteOperator
or_all arguments = apply_for_all arguments orer True

greaterer :: BinaryOperator
greaterer x y = comparison (>) x y

greater :: InfiniteOperator
greater arguments = apply_for_all arguments greaterer False

lesser :: BinaryOperator
lesser x y = comparison (<) x y

less :: InfiniteOperator
less arguments = apply_for_all arguments lesser False

equaler :: BinaryOperator
equaler x y = comparison (==) x y

equal :: InfiniteOperator
equal arguments = apply_for_all arguments equaler False
