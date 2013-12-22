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


class VariableNode(Node):
    def __init__(self, name):
        Node.__init__(self, name)


class BindingNode(Node):
    def __init__(self, variable_name, binding_expr, body_expr):
        Node.__init__(self, 'define')
        self.variable_name = variable_name
        self.binding_expr = binding_expr
        self.body_expr = body_expr


class LambdaNode(Node):
    def __init__(self, body, arguments, remaining_args):
        Node.__init__(self, 'lambda')
        self.body = body
        self.arguments = arguments
        self.remaining_args = remaining_args


class FunctionCallNode(Node):
    def __init__(self, name, operands):
        Node.__init__(self, name)
        self.operands = operands


class StructDeclaration(Node):
    def __init__(self, members):
        Node.__init__(self, 'struct')
        self.members = members


class MemberAccessNode(Node):
    def __init__(self, struct_name, member_name):
        Node.__init__(self, 'member')
        self.struct_name = struct_name
        self.member_name = member_name


class ConditionalNode(Node):
    def __init__(self, if_expr, then_expr, else_expr):
        Node.__init__(self, 'if')
        self.if_expr = if_expr
        self.then_expr = then_expr
        self.else_expr = else_expr


class NegateNode(Node):
    def __init__(self, expr):
        Node.__init__(self, 'not')
        self.expr = expr
