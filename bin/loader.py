#!/usr/bin/env python
import sys

from src import parse
from src import ast
from src import evaluation
from src import main


def load_and_run(filename):
    with open(filename, 'r') as f:
        print('loading file...')
        contents = ''.join(f.readlines())
        print(contents)
        print('building ast...')
        loaded_ast = parse.Parser(contents).parse_expression()
        print(ast.draw_node(loaded_ast))
        print('running...')
        print(evaluation.evaluate(loaded_ast, main.DefaultEnvironment()))

if __name__ == '__main__':
    load_and_run(sys.argv[1])
