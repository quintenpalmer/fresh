import ast


def draw_node(astNode):
    def get_name(a):
        if isinstance(a, str):
            return a
        elif isinstance(a, ast.Node):
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
