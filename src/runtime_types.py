class RunTimeType(object):
    def __init__(self, val, conv):
        self.val = conv(val)

    def __repr__(self):
        return str(self.val)


class IntType(RunTimeType):
    def __init__(self, val):
        RunTimeType.__init__(self, val, int)


class BoolType(RunTimeType):
    def __init__(self, val):
        RunTimeType.__init__(self, val, bool)


class ClosureType(RunTimeType):
    def __init__(self, env, body, arguments):
        self.env = env
        self.body = body
        self.arguments = arguments
