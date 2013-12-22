import test_examples

from src import environment
from src import evaluation
from src import parse


for example in test_examples.examples:
    example_parser = parse.Parser(example.expr)
    parsed = evaluation.evaluate(example_parser.parse_expression(), environment.DefaultEnvironment())
    print(test_examples.format_message(parsed.val, example.evaluation, example.expr))
    if parsed.val != example.evaluation:
        raise Exception('BAD BAD BAD BAD')
print('OK')
