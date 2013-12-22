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
