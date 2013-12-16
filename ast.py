import types


class Node:
    def __init__(self, name):
        self.name = name

    def __repr__(self):
        variables = dict(vars(self))
        variables.pop('name', None)
        if variables:
            ret = '(' + self.name + ' '
            for arg in variables.values():
                if not isinstance(arg, types.FunctionType):
                    if isinstance(arg, tuple):
                        ret += ' '.join(map(repr, arg)) + ' '
                    else:
                        ret += arg.__repr__() + ' '
            ret = ret[:-1]
            ret += ')'
        else:
            ret = self.name
        return ret


class RootNode(Node):
    def __init__(self, name):
        Node.__init__(self, name)

    def evaluate(self):
        return self.name


class ArithmaticNode(Node):
    def __init__(self, name, operation, *operands):
        Node.__init__(self, name)
        self.operation = operation
        self.operands = operands

    def evaluate(self):
        cumulation = int(self.operands[0].evaluate())
        for arg in self.operands[1:]:
            cumulation = self.operation(cumulation, int(arg.evaluate()))
        return cumulation


class ConditionalNode(Node):
    def __init__(self, name, if_expr, then_expr, else_expr):
        Node.__init__(self, name)
        self.if_expr = if_expr
        self.then_expr = then_expr
        self.else_expr = else_expr

    def evaluate(self):
        if self.if_expr.evaluate():
            return self.then_expr.evaluate()
        else:
            return self.else_expr.evaluate()


class ComparisonNode(Node):
    def __init__(self, name, first_expr, second_expr, comparison):
        Node.__init__(self, name)
        self.first_expr = first_expr
        self.second_expr = second_expr
        self.comparison = comparison

    def evaluate(self):
        if self.comparison(
                int(self.first_expr.evaluate()),
                int(self.second_expr.evaluate())):
            return 'true'
        else:
            return 'false'


class NegateNode(Node):
    def __init__(self, expr):
        Node.__init__(self, 'not')
        self.expr = expr

    def evaluate(self):
        evaluated_expr = self.expr.evaluate()
        if evaluated_expr == 'true':
            return 'false'
        elif evaluated_expr == 'false':
            return 'true'
        else:
            raise Exception('expected boolean, got %s' % evaluated_expr)
