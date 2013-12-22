import tokenize
import ast
import runtime

bools = ['true', 'false']


def bool_from_string(bool_string):
    if bool_string == 'true':
        return runtime.BoolType(True)
    elif bool_string == 'false':
        return runtime.BoolType(False)
    else:
        raise Exception("not a bool string_literal %s" % bool_string)


class Parser:
    def __init__(self, source_string):
        self.tokenizer = tokenize.Tokenizer(source_string)
        self.function_map = {
            'if': self.conditional,
            'not': self.negate,
            'define': self.define,
            'lambda': self.lambda_,
            'struct': self.struct,
            'member': self.member,
        }

    def parse_expression(self):
        current = self.tokenizer.chomp()
        if current == '(':
            return self.func_call()
        elif current.isdigit() or (current[1:].isdigit() and current[0] == '-'):
            return ast.LeafNode(current, runtime.IntType)
        elif current in bools:
            return ast.LeafNode(current, bool_from_string)
        else:
            return ast.VariableNode(current)

    def func_call(self):
        func_name = self.tokenizer.chomp()
        if func_name in self.function_map:
            func = self.function_map[func_name]()
            if func_name != 'define':
                close = self.tokenizer.chomp()
            return func
        else:
            operands = []
            while True:
                if self.tokenizer.peek() == ')':
                    break
                to_add = self.parse_expression()
                operands.append(to_add)
            close = self.tokenizer.chomp()
            return ast.FunctionCallNode(func_name, operands)

    def define(self):
        name = self.tokenizer.chomp()
        expression = self.parse_expression()
        close = self.tokenizer.chomp()
        body = self.parse_expression()
        return ast.BindingNode(name, expression, body)

    def conditional(self):
        cond_expr = self.parse_expression()
        then_expr = self.parse_expression()
        else_expr = self.parse_expression()
        return ast.ConditionalNode(cond_expr, then_expr, else_expr)

    def negate(self):
        to_negate_expr = self.parse_expression()
        return ast.NegateNode(to_negate_expr)

    def lambda_(self):
        remaining_args = None
        open_bracket = self.tokenizer.chomp()
        args = []
        while True:
            if self.tokenizer.peek() == ']':
                break
            if self.tokenizer.peek() == '...':
                remaining_args = self.tokenizer.chomp()
                break
            arg = self.tokenizer.chomp()
            args.append(arg)
        close_bracket = self.tokenizer.chomp()
        body = self.parse_expression()
        return ast.LambdaNode(body, args, remaining_args)

    def struct(self):
        members = []
        while True:
            if self.tokenizer.peek() == ')':
                break
            member = self.tokenizer.chomp()
            members.append(member)
        return ast.StructDeclaration(members)

    def member(self):
        struct_name = self.tokenizer.chomp()
        member_name = self.tokenizer.chomp()
        return ast.MemberAccessNode(struct_name, member_name)
