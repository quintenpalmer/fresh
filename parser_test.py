import parser
import test_examples


for example in test_examples.examples:
    parsed = parser.parse_expression(example.expr)[0].evaluate()
    print parsed == example.evaluation, parsed, example.evaluation, example.expr
    if parsed != example.evaluation:
        raise Exception('BAD BAD BAD BAD')
print 'OK'
