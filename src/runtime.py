class RunTimeType(object):
    def __init__(self, val):
        self.val = val

    def __repr__(self):
        return str(self.val)


class IntType(RunTimeType):
    def __init__(self, val):
        RunTimeType.__init__(self, int(val))


class BoolType(RunTimeType):
    def __init__(self, val):
        RunTimeType.__init__(self, bool(val))


class BuiltinClosureType(RunTimeType):
    def __init__(self, operation, arguments, remaining_args):
        self.operation = operation
        self.arguments = arguments
        self.remaining_args = remaining_args

    def __repr__(self):
        return 'builtindef'


class ClosureType(RunTimeType):
    def __init__(self, env, body, arguments, remaining_args):
        self.env = env
        self.body = body
        self.arguments = arguments
        self.remaining_args = remaining_args

    def __repr__(self):
        return 'closure'


class StructDeclarationType(RunTimeType):
    def __init__(self, members):
        self.members = members

    def __repr__(self):
        return 'strdecl' + str(self.members)


class StructInstantiationType(RunTimeType):
    def __init__(self, struct_type, arguments):
        self.values = {val: key for val, key in zip(
            struct_type.members, arguments)}

    def __repr__(self):
        return 'strinst' + str(self.values)
