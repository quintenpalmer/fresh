import sys
import re

from src import parser
from src import environment

def load_and_run(filename):
    with open(filename, 'r') as f:
        print('loading file...')
        contents = ''.join(f.readlines())
        print(contents)
        print('building ast...')
        ast = parser.Parser(contents).parse_expression()
        print(ast)
        print('running...')
        print(ast.evaluate(environment.Environment()))

if __name__ == '__main__':
    load_and_run(sys.argv[1])
