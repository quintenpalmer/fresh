#!/usr/bin/env python
from src import parse
from src import evaluation


def _read():
    return raw_input(' > ')


def _eval(string):
    return evaluate(parser.Parser(string).parse_expression())


def _print(string):
    print(str(string) + '\n')


def _loop(none=None):
    _loop(_print(_eval(_read())))


if __name__ == '__main__':
    #_loop()
    while True:
        print(evaluation.evaluate(parse.Parser(raw_input(' > ')).parse_expression()))
