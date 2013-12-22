import runtime
import types

def get_name(a):
    if isinstance(a, str):
        return a
    else:
        return repr(a)

class Node(object):
    def __init__(self, name):
        self.name = name

    def __repr__(self):
        variables = dict(vars(self))
        ret = ''
        variables.pop('name', None)
        variables.pop('type_', None)
        if variables:
            ret += '(' + self.name + ' '
            for arg in variables.values():
                if not isinstance(arg, types.FunctionType):
                    if isinstance(arg, list):
                        ret += ' '.join(map(get_name, arg)) + ' '
                    else:
                        ret += get_name(arg) + ' '
            ret = ret[:-1]
            ret += ')'
        else:
            ret = self.name
        return ret


class LeafNode(Node):
    def __init__(self, name, type_):
        Node.__init__(self, name)
        self.type_ = type_

    def evaluate(self, env):
        return self.type_(self.name)


class VariableNode(Node):
    def __init__(self, name):
        Node.__init__(self, name)

    def evaluate(self, env):
        if self.name in env.dictionary:
            return env.dictionary[self.name]
        else:
            raise Exception("Variable not found \"%s\"" % self.name)


class BindingNode(Node):
    def __init__(self, variable_name, binding_expr, body_expr):
        Node.__init__(self, 'define')
        self.variable_name = variable_name
        self.binding_expr = binding_expr
        self.body_expr = body_expr

    def evaluate(self, env):
        env = env.copy_with(
            {self.variable_name: self.binding_expr.evaluate(env)})
        return self.body_expr.evaluate(env)


class LambdaNode(Node):
    def __init__(self, body, arguments):
        Node.__init__(self, 'lambda')
        self.body = body
        self.arguments = arguments

    def evaluate(self, env):
        return runtime.ClosureType(env, self.body, self.arguments)


class FunctionCallNode(Node):
    def __init__(self, name, operands):
        Node.__init__(self, name)
        self.operands = operands

    def evaluate(self, env):
        if self.name in env.dictionary:
            value = env.dictionary[self.name]
            if isinstance(value, runtime.ClosureType):
                lambda_env = env.copy_with(value.env.dictionary)
                for variable, operand in zip(value.arguments, self.operands):
                    lambda_env = lambda_env.copy_with({variable: operand.evaluate(env)})
                return value.body.evaluate(lambda_env)
            elif isinstance(value, runtime.StructDeclarationType):
                return runtime.StructInstantiationType(value, map(lambda operand: operand.evaluate(env), self.operands))
            else:
                return value.evaluate(env, self.operands)
        else:
            raise Exception("Function declaration not found \"%s\"" % self.name)


class StructDeclaration(Node):
    def __init__(self, members):
        Node.__init__(self, 'struct')
        self.members = members

    def evaluate(self, env):
        return runtime.StructDeclarationType(self.members)


class MemberAccessNode(Node):
    def __init__(self, struct_name, member_name):
        Node.__init__(self, 'member')
        self.struct_name = struct_name
        self.member_name = member_name

    def evaluate(self, env):
        if self.struct_name in env.dictionary:
            struct = env.dictionary[self.struct_name]
            if self.member_name in struct.values:
                return struct.values[self.member_name]
            else:
                raise Exception("Struct %s does not have member %s" % (
                    self.struct_name, self.member_name))
        else:
            raise Exception("Struct %s not found" % self.member_name)


class BuiltinCallNode(Node):
    def __init__(self, name, accumulative, operation):
        Node.__init__(self, name)
        self.accumulative = accumulative
        self.operation = operation

    def evaluate(self, env, operands):
        result = operands[0].evaluate(env)
        previous = result
        for arg in operands[1:]:
            evaluation = arg.evaluate(env)
            result = self.operation(previous, evaluation)
            if result.val is False:
                return result
            if self.accumulative:
                previous = result
            else:
                previous = evaluation
        return result


class ConditionalNode(Node):
    def __init__(self, if_expr, then_expr, else_expr):
        Node.__init__(self, 'if')
        self.if_expr = if_expr
        self.then_expr = then_expr
        self.else_expr = else_expr

    def evaluate(self, env):
        if self.if_expr.evaluate(env).val is True:
            return self.then_expr.evaluate(env)
        else:
            return self.else_expr.evaluate(env)


class NegateNode(Node):
    def __init__(self, expr):
        Node.__init__(self, 'not')
        self.expr = expr

    def evaluate(self, env):
        evaluated_expr = self.expr.evaluate(env)
        if evaluated_expr.val is True:
            return runtime.BoolType(False)
        elif evaluated_expr.val is False:
            return runtime.BoolType(True)
        else:
            raise Exception('expected boolean, got %s' % evaluated_expr)
