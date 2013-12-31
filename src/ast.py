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


class GetEnvironmentBindingNode(Node):
    def __init__(self):
        Node.__init__(self, 'getenv')


class LoadingNode(Node):
    def __init__(self, binding_name, body):
        Node.__init__(self, 'loader')
        self.binding_name = binding_name
        self.body = body
