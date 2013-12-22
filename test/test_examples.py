def get_correctness_message(boolean):
    if boolean:
        return "was"
    else:
        return "wasn't"


def pre_pad(message):
    return ' '*(6-len(str(message))) + str(message)


def format_message(result, expected, original):
    return '%s should be %s and it %s source: %s' % (
        pre_pad(result),
        pre_pad(expected),
        get_correctness_message(result == expected),
        original)

class TestExpression:
    def __init__(self, expr, evaluation, tokenized):
        self.expr = expr
        self.evaluation = evaluation
        self.tokenized = tokenized


examples = [
    TestExpression(
        '4', 4,
        ['4']),
    TestExpression(
        'true', True,
        ['true']),
    TestExpression(
        '(+ 3 4)', 7,
        ['(', '+', '3', '4', ')']),
    TestExpression(
        '(- 2 1)', 1,
        ['(', '-', '2', '1', ')']),
    TestExpression(
        '(+ 21 1)', 22,
        ['(', '+', '21', '1', ')']),
    TestExpression(
        '(* 2 8)', 16,
        ['(', '*', '2', '8', ')']),
    TestExpression(
        '(- 0 8)', -8,
        ['(', '-', '0', '8', ')']),
    TestExpression(
        '(+ (+ 2 8) 5)', 15,
        ['(', '+', '(', '+', '2', '8', ')', '5', ')']),
    TestExpression(
        '(+ (- 8 2) 5)', 11,
        ['(', '+', '(', '-', '8', '2', ')', '5', ')']),
    TestExpression(
        '(+ (- 8 2) 5 (* 2 2))', 15,
        ['(', '+', '(', '-', '8', '2', ')', '5', '(', '*', '2', '2', ')', ')']),
    TestExpression(
        '(+ (- 8 (+ 1 1)) 5 (* 2 2))', 15,
        ['(', '+', '(', '-', '8', '(', '+', '1', '1', ')', ')', '5', '(', '*', '2', '2', ')',
         ')']),
    TestExpression(
        '(+ 3 3 3 (+ 3 (- 3 4 4) 3 (- 3 2)) (* 3 2) (- 1 7))', 11,
        ['(', '+', '3', '3', '3', '(', '+', '3', '(', '-', '3', '4', '4', ')', '3', '(', '-', '3',
         '2', ')', ')', '(', '*', '3', '2', ')', '(', '-', '1', '7', ')', ')']),
    TestExpression(
        '(if true (+ 4 2) (+ 3 2))', 6,
        ['(', 'if', 'true', '(', '+', '4', '2', ')', '(', '+', '3', '2', ')', ')']),
    TestExpression(
        '(if (> 4 3) (+ 4 2) (+ 3 2))', 6,
        ['(', 'if', '(', '>', '4', '3', ')', '(', '+', '4', '2', ')', '(', '+', '3', '2', ')',
         ')']),
    TestExpression(
        '(+ 3 (if (> 4 3) (+ 4 2) (+ 3 2)))', 9,
        ['(', '+', '3', '(', 'if', '(', '>', '4', '3', ')', '(', '+', '4', '2', ')', '(', '+', '3',
         '2', ')', ')', ')']),
    TestExpression(
        '(> 5 4)', True,
        ['(', '>', '5', '4', ')']),
    TestExpression(
        '(> 5 4 3)', True,
        ['(', '>', '5', '4', '3', ')']),
    TestExpression(
        '(> 5 (+ 3 4))', False,
        ['(', '>', '5', '(', '+', '3', '4', ')', ')']),
    TestExpression(
        '(< 5 4)', False,
        ['(', '<', '5', '4', ')']),
    TestExpression(
        '(< 5 4 3)', False,
        ['(', '<', '5', '4', '3', ')']),
    TestExpression(
        '(> 5 4 5)', False,
        ['(', '>', '5', '4', '5', ')']),
    TestExpression(
        '(= 5 5 5)', True,
        ['(', '=', '5', '5', '5', ')']),
    TestExpression(
        '(= 5 5 6)', False,
        ['(', '=', '5', '5', '6', ')']),
    TestExpression(
        '(= 5 4 4)', False,
        ['(', '=', '5', '4', '4', ')']),
    TestExpression(
        '(< 5 (+ 3 4))', True,
        ['(', '<', '5', '(', '+', '3', '4', ')', ')']),
    TestExpression(
        '(= 5 5)', True,
        ['(', '=', '5', '5', ')']),
    TestExpression(
        '(= 5 (+ 3 4))', False,
        ['(', '=', '5', '(', '+', '3', '4', ')', ')']),
    TestExpression(
        '(not true)', False,
        ['(', 'not', 'true', ')']),
    TestExpression(
        '(not false)', True,
        ['(', 'not', 'false', ')']),
    TestExpression(
        '(not (> 4 5))', True,
        ['(', 'not', '(', '>', '4', '5', ')', ')']),
    TestExpression(
        '(if (not (> 4 5)) 5 6)', 5,
        ['(', 'if', '(', 'not', '(', '>', '4', '5', ')', ')', '5', '6', ')']),
    TestExpression(
        '(or true true)', True,
        ['(', 'or', 'true', 'true', ')']),
    TestExpression(
        '(or false true)', True,
        ['(', 'or', 'false', 'true', ')']),
    TestExpression(
        '(or false false)', False,
        ['(', 'or', 'false', 'false', ')']),
    TestExpression(
        '(and true true)', True,
        ['(', 'and', 'true', 'true', ')']),
    TestExpression(
        '(and false true)', False,
        ['(', 'and', 'false', 'true', ')']),
    TestExpression(
        '(and false false)', False,
        ['(', 'and', 'false', 'false', ')']),
    TestExpression(
        '(define a 4)(+ 3 a)', 7,
        ['(', 'define', 'a', '4', ')', '(', '+', '3', 'a', ')']),
    TestExpression(
        '(define a (+ 3 4))(+ 3 a)', 10,
        ['(', 'define', 'a', '(', '+', '3', '4', ')', ')', '(', '+', '3', 'a', ')']),
    TestExpression(
        '(define a 4)(define b 5)(+ b a)', 9,
        ['(', 'define', 'a', '4', ')', '(', 'define', 'b', '5', ')', '(', '+', 'b', 'a', ')']),
    TestExpression(
        '(define square (lambda [i] (* i i)))(square 5)', 25,
        ['(', 'define', 'square', '(', 'lambda', '[', 'i', ']', '(', '*', 'i', 'i', ')', ')', ')', '(', 'square', '5', ')']),
    TestExpression(
        '(define _fib (lambda [count current old] (if (< count 0) current (_fib (- count 1) (+ current old) current))))(define fib (lambda [index] (_fib index 1 0)))(fib 5)', 13,
['(', 'define', '_fib', '(', 'lambda', '[', 'count', 'current', 'old', ']', '(', 'if', '(', '<', 'count', '0', ')', 'current', '(', '_fib', '(', '-', 'count', '1', ')', '(', '+', 'current', 'old', ')', 'current', ')', ')', ')', ')', '(', 'define', 'fib', '(', 'lambda', '[', 'index', ']', '(', '_fib', 'index', '1', '0', ')', ')', ')', '(', 'fib', '5', ')']),
    TestExpression(
        '(define person (struct height))(define quinten (person 10))(member quinten height)', 10,
        ['(', 'define', 'person', '(', 'struct', 'height', ')', ')', '(', 'define', 'quinten', '(', 'person', '10', ')', ')', '(', 'member', 'quinten', 'height', ')']),
    TestExpression(
        '(define list (struct element tail_list))(define my_list (list 10 (list 5 0)))(member my_list element)', 10,
        ['(', 'define', 'list', '(', 'struct', 'element', 'tail_list', ')', ')', '(', 'define', 'my_list', '(', 'list', '10', '(', 'list', '5', '0', ')', ')', ')', '(', 'member', 'my_list', 'element', ')']),
]
