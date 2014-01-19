import test_examples

from src import parse

for example in test_examples.examples:

    example_parser = parse.Parser(example.expr)
    ast = example_parser.parse_expression()

    printed = ast.__repr__()
    print(test_examples.format_message(printed, example.expr, example.expr))
    if printed != example.expr:
        raise Exception('BAD BAD BAD BAD')
print('OK')
