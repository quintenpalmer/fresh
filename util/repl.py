from src import parser
from src import environment


def _read():
    return raw_input(' > ')


def _eval(string):
    return parser.Parser(string).parse_expression().evaluate(environment.Environment())


def _print(string):
    print(str(string) + '\n')


def _loop(none=None):
    _loop(_print(_eval(_read())))


if __name__ == '__main__':
    #_loop()
    while True:
        print(parser.Parser(raw_input(' > ')).parse_expression().evaluate(environment.Environment()))
