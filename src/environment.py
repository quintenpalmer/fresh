import runtime
import ast


def add():
    def adder(base, addition):
        return runtime.IntType(base.val + addition.val)
    return ast.BuiltinNode('+', True, adder)

def subtract():
    def subtracted(base, addition):
        return runtime.IntType(base.val - addition.val)

    return ast.BuiltinNode('-', True, subtracted)

def multiply():
    def multiplied(base, addition):
        return runtime.IntType(base.val * addition.val)

    return ast.BuiltinNode('*', True, multiplied)

def or_():
    def or_two(first, second):
        return runtime.BoolType(first.val or second.val)

    return ast.BuiltinNode('or', True, or_two)

def and_():
    def and_two(first, second):
        return runtime.BoolType(first.val and second.val)

    return ast.BuiltinNode('and', True, and_two)

def equal_to():
    def equals(first, second):
        return runtime.BoolType(first.val == second.val)

    return ast.BuiltinNode('=', False, equals)

def less_than():
    def is_less(first, second):
        return runtime.BoolType(first.val < second.val)

    return ast.BuiltinNode('<', False, is_less)

def greater():
    def is_greater(first, second):
        return runtime.BoolType(first.val > second.val)

    return ast.BuiltinNode('>', False, is_greater)


default_binding = {
    '+': add(),
    '-': subtract(),
    '*': multiply(),
    '>': greater(),
    '<': less_than(),
    '=': equal_to(),
    'and': and_(),
    'or': or_(),
}

class Environment:
    def __init__(self, dictionary=default_binding):
        self.dictionary = dictionary

    def copy_with(self, new_environment):
        env = Environment(dict(self.dictionary))
        env.dictionary.update(new_environment)
        return env
