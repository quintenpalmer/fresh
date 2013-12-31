import runtime


def multi_operand_helper(name, accumulative, operation):
    def _multi_operand_helper(operands, remaining_operands):
        if len(operands) != 0:
            raise Exception("multi_operand_helper %s takes no arguments" % name)
        result = remaining_operands[0]
        previous = result
        for arg in remaining_operands[1:]:
            result = operation(previous, arg)
            if result.val is False:
                return result
            if accumulative:
                previous = result
            else:
                previous = arg
        return result
    return _multi_operand_helper


def add():
    def adder(base, addition):
        return runtime.IntType(base.val + addition.val)
    return runtime.BuiltinClosureType(multi_operand_helper('+', True, adder), [], 'args')


def subtract():
    def subtracted(base, addition):
        return runtime.IntType(base.val - addition.val)

    return runtime.BuiltinClosureType(multi_operand_helper('-', True, subtracted), [], 'args')


def multiply():
    def multiplied(base, addition):
        return runtime.IntType(base.val * addition.val)

    return runtime.BuiltinClosureType(multi_operand_helper('*', True, multiplied), [], 'args')


def or_():
    def or_two(first, second):
        return runtime.BoolType(first.val or second.val)

    return runtime.BuiltinClosureType(multi_operand_helper('or', True, or_two), [], 'args')


def and_():
    def and_two(first, second):
        return runtime.BoolType(first.val and second.val)

    return runtime.BuiltinClosureType(multi_operand_helper('and', True, and_two), [], 'args')


def equal_to():
    def equals(first, second):
        return runtime.BoolType(first.val == second.val)

    return runtime.BuiltinClosureType(multi_operand_helper('=', False, equals), [], 'args')


def less_than():
    def is_less(first, second):
        return runtime.BoolType(first.val < second.val)

    return runtime.BuiltinClosureType(multi_operand_helper('<', False, is_less), [], 'args')


def greater():
    def is_greater(first, second):
        return runtime.BoolType(first.val > second.val)

    return runtime.BuiltinClosureType(multi_operand_helper('>', False, is_greater), [], 'args')
