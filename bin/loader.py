#!/usr/bin/env python
import sys

from src import parse
from src import evaluation

def load_and_run(filename):
    with open(filename, 'r') as f:
        print('loading file...')
        contents = ''.join(f.readlines())
        print(contents)
        print('building ast...')
        ast = parse.Parser(contents).parse_expression()
        print(ast)
        print('running...')
        print(evaluation.evaluate(ast))

if __name__ == '__main__':
    load_and_run(sys.argv[1])
