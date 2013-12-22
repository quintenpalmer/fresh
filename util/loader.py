import sys
import re

from src import parse
from src import environment

def load_and_run(filename):
    with open(filename, 'r') as f:
        print('loading file...')
        contents = ''.join(f.readlines())
        print(contents)
        print('building ast...')
        ast = parse.Parser(contents).parse_expression()
        print(ast)
        print('running...')
        print(ast.evaluate(environment.DefaultEnvironment()))

if __name__ == '__main__':
    load_and_run(sys.argv[1])
