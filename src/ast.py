class Node(object):
    def __init__(self, name):
        self.name = name


class IntNode(Node):
    def __init__(self, name):
        Node.__init__(self, name)


class BoolNode(Node):
    def __init__(self, name):
        Node.__init__(self, name)


class VariableNode(Node):
    def __init__(self, name):
        Node.__init__(self, name)


class ConditionalNode(Node):
    def __init__(self, if_expr, then_expr, else_expr):
        Node.__init__(self, 'if')
        self.if_expr = if_expr
        self.then_expr = then_expr
        self.else_expr = else_expr


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


class StructDeclarationNode(Node):
    def __init__(self, members):
        Node.__init__(self, 'struct')
        self.members = members


class MemberAccessNode(Node):
    def __init__(self, struct_name, member_name):
        Node.__init__(self, 'member')
        self.struct_name = struct_name
        self.member_name = member_name


class NegateNode(Node):
    def __init__(self, expr):
        Node.__init__(self, 'not')
        self.expr = expr


def draw_node(astNode):
    def get_name(a):
        if isinstance(a, str):
            return a
        elif isinstance(a, Node):
            return draw_node(a)
        else:
            return repr(a)

    variables = dict(vars(astNode))
    ret = ''
    variables.pop('name', None)
    variables.pop('type_', None)
    if variables:
        ret += '(' + astNode.name + ' '
        for arg in variables.values():
            if isinstance(arg, list):
                ret += ' '.join(map(get_name, arg)) + ' '
            else:
                ret += get_name(arg) + ' '
        ret = ret[:-1]
        ret += ')'
    else:
        ret = astNode.name
    return ret
