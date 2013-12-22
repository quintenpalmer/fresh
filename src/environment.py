import builtin


class Environment:
    def __init__(self, dictionary={}):
        self.dictionary = dictionary

    def copy_with(self, new_environment):
        env = Environment(dict(self.dictionary))
        env.dictionary.update(new_environment)
        return env


default_binding = {
    '+': builtin.add(),
    '-': builtin.subtract(),
    '*': builtin.multiply(),
    '>': builtin.greater(),
    '<': builtin.less_than(),
    '=': builtin.equal_to(),
    'and': builtin.and_(),
    'or': builtin.or_(),
}


def DefaultEnvironment():
    return Environment(dictionary=default_binding)
