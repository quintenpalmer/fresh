import parser
import test_examples

for example in test_examples.examples:
    ast = parser.parse_expression(example.expr)[0]
    printed = ast.__repr__()
    print printed == example.expr, printed, example.expr
    if printed != example.expr:
        raise Exception('BAD BAD BAD BAD')
print 'OK'
