import parse
import evaluation
import builtin
import environment


pre_env = environment.Environment({
    '+': builtin.add(),
    '-': builtin.subtract(),
    '*': builtin.multiply(),
    '>': builtin.greater(),
    '<': builtin.less_than(),
    '=': builtin.equal_to(),
    'and': builtin.and_(),
    'or': builtin.or_(),
})

def add_module_to_env(name, env):
    with open('lib/%s.fr' % name) as f:
        raw_string = ''.join(f.readlines())
    imported_ast = parse.Parser(raw_string).parse_expression()
    imported_val = evaluation.evaluate(imported_ast, env)
    imported_env = env.copy_with(imported_val.env.dictionary)
    return imported_env

default_binding = add_module_to_env('builtin', pre_env).dictionary


def DefaultEnvironment():
    return environment.Environment(dictionary=default_binding)
