class Environment:
    def __init__(self, dictionary={}):
        self.dictionary = dictionary

    def copy_with(self, new_environment):
        env = Environment(dict(self.dictionary))
        env.dictionary.update(new_environment)
        return env
