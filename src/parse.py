import tokenize
import ast

bools = ['true', 'false']


class Parser:
    def __init__(self, source_string):
        self.tokenizer = tokenize.Tokenizer(source_string)
        self.function_map = {
            'if': self.if_,
            'define': self.define,
            'lambda': self.lambda_,
            'struct': self.struct,
            'member': self.member,
            'with': self.with_,
        }

    def parse_expression(self):
        current = self.tokenizer.chomp()
        if current == '(':
            return self.func_call()
        elif current == tokenize.EOF:
            raise ParsingError("Expecting an expression expecting %s" % tokenize.EOF)
        elif current.isdigit() or (current[1:].isdigit() and current[0] == '-'):
            return ast.IntNode(current)
        elif current in bools:
            return ast.BoolNode(current)
        else:
            return ast.VariableNode(current)

    def maybe_parse_expression(self):
        if self.tokenizer.peek() == tokenize.EOF:
            return ast.GetEnvironmentBindingNode()
        else:
            return self.parse_expression()

    def func_call(self):
        func_name = self.tokenizer.chomp()
        if func_name in self.function_map:
            func = self.function_map[func_name]()
            if func_name not in ('define', 'with'):
                assert_close_expression(self.tokenizer.chomp())
            return func
        else:
            operands = []
            while True:
                if self.tokenizer.peek() == ')':
                    break
                operands.append(self.parse_expression())
            assert_close_expression(self.tokenizer.chomp())
            return ast.FunctionCallNode(func_name, operands)

    def define(self):
        name = self.tokenizer.chomp()
        expression = self.parse_expression()
        assert_close_expression(self.tokenizer.chomp())
        body = self.maybe_parse_expression()
        return ast.BindingNode(name, expression, body)

    def if_(self):
        cond_expr = self.parse_expression()
        then_expr = self.parse_expression()
        else_expr = self.parse_expression()
        return ast.ConditionalNode(cond_expr, then_expr, else_expr)

    def lambda_(self):
        remaining_args = None
        open_bracket = self.tokenizer.chomp()
        args = []
        while True:
            if self.tokenizer.peek() == ']':
                break
            if self.tokenizer.peek() == '...':
                assert_elipsis(self.tokenizer.chomp())
                remaining_args = self.tokenizer.chomp()
                break
            args.append(self.tokenizer.chomp())
        assert_close_params(self.tokenizer.chomp())
        body = self.parse_expression()
        return ast.LambdaNode(body, args, remaining_args)

    def struct(self):
        members = []
        while True:
            if self.tokenizer.peek() == ')':
                break
            members.append(self.tokenizer.chomp())
        return ast.StructDeclarationNode(members)

    def member(self):
        struct_name = self.tokenizer.chomp()
        member_name = self.tokenizer.chomp()
        return ast.MemberAccessNode(struct_name, member_name)

    def with_(self):
        module_name = self.tokenizer.chomp()
        assert_close_expression(self.tokenizer.chomp())
        body = self.maybe_parse_expression()
        return ast.LoadingNode(module_name, body)


class ParsingError(Exception):
    pass


def assert_close_expression(token):
    _assert_is_token(token, ')')


def assert_close_params(token):
    _assert_is_token(token, ']')


def assert_elipsis(token):
    _assert_is_token(token, '...')


def _assert_is_token(to_check, expected):
    if to_check != expected:
        raise ParsingError("Expecting %s found %s" % (expected, to_check))
