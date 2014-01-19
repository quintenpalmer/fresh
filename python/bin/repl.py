#!/usr/bin/env python
from src import parse
from src import evaluation
from src import main
from src import runtime


def _read():
    return raw_input(' $ ')


def _eval(string, env):
    return evaluation.evaluate(parse.Parser(string).parse_expression(), env)


def _print(string):
    print(str(string))


def _loop(env=main.DefaultEnvironment()):
    while True:
        evaluation = _eval(_read(), env)
        if isinstance(evaluation, runtime.BindingType):
            env = evaluation.env
        _print(evaluation)


if __name__ == '__main__':
    _loop()
