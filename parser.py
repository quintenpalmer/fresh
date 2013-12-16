import tokenizer
import ast

bools = ['true', 'false']


def parse_expression(remaining):
    current, remaining = tokenizer.chomp(remaining)
    if current == '(':
        return func_call(remaining)
    elif current.isdigit():
        return ast.RootNode(current), remaining
    elif current in bools:
        return ast.RootNode(current), remaining
    else:
        raise Exception('invalid char %s' % current)


def func_call(remaining):
    current, remaining = tokenizer.chomp(remaining)
    if current in function_map:
        return function_map[current](remaining)
    else:
        raise Exception('%s is not supported' % current)


def negate(remaining):
    to_negate_expr, remaining = parse_expression(remaining)
    close, remaining = tokenizer.chomp(remaining)
    return ast.NegateNode(to_negate_expr), remaining


def equal_to(remaining):
    def equals(first, second):
        return first == second

    return int_comp(remaining, '=', equals)


def less_than(remaining):
    def is_less(first, second):
        return first < second

    return int_comp(remaining, '<', is_less)


def greater(remaining):
    def is_greater(first, second):
        return first > second

    return int_comp(remaining, '>', is_greater)


def int_comp(remaining, name, comparison):
    first_expr, remaining = parse_expression(remaining)
    second_expr, remaining = parse_expression(remaining)
    close, remaining = tokenizer.chomp(remaining)
    return ast.ComparisonNode(name, first_expr, second_expr, comparison), remaining


def conditional(remaining):
    cond_expr, remaining = parse_expression(remaining)
    #cond_expr = bool(cond_expr)
    then_expr, remaining = parse_expression(remaining)
    else_expr, remaining = parse_expression(remaining)
    close, remaining = tokenizer.chomp(remaining)
    return ast.ConditionalNode('if', cond_expr, then_expr, else_expr), remaining


def add(remaining):
    def added(base, addition):
        return base + addition

    return arithmatic(remaining, '+', added)


def subtract(remaining):
    def subtracted(base, addition):
        return base - addition

    return arithmatic(remaining, '-', subtracted)


def multiply(remaining):
    def multiplied(base, addition):
        return base * addition

    return arithmatic(remaining, '*', multiplied)


def arithmatic(remaining, name, operation):
    cumulation, remaining = parse_expression(remaining)
    operands = [cumulation]
    while True:
        current = tokenizer.peek(remaining)
        if current == ')':
            close, remaining = tokenizer.chomp(remaining)
            break
        to_add, remaining = parse_expression(remaining)
        operands.append(to_add)
    return ast.ArithmaticNode(name, operation, *operands), remaining


function_map = {
    '+': add,
    '-': subtract,
    '*': multiply,
    '>': greater,
    '<': less_than,
    '=': equal_to,
    'if': conditional,
    'not': negate,
}
