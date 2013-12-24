import environment
import ast
import runtime


def evaluate(self, env=environment.DefaultEnvironment()):
    if isinstance(self, ast.IntNode):
        return runtime.IntType(self.name)

    elif isinstance(self, ast.BoolNode):
        def bool_from_string(bool_string):
            if bool_string == 'true':
                return runtime.BoolType(True)
            elif bool_string == 'false':
                return runtime.BoolType(False)
            else:
                raise Exception("not a bool string_literal %s" % bool_string)
        return bool_from_string(self.name)

    elif isinstance(self, ast.VariableNode):
        if self.name in env.dictionary:
            return env.dictionary[self.name]
        else:
            raise Exception("Variable not found \"%s\"" % self.name)

    elif isinstance(self, ast.BindingNode):
        env = env.copy_with(
            {self.variable_name: evaluate(self.binding_expr, env)})
        return evaluate(self.body_expr, env)

    elif isinstance(self, ast.LambdaNode):
        return runtime.ClosureType(env, self.body, self.arguments, self.remaining_args)

    elif isinstance(self, ast.FunctionCallNode):
        if self.name in env.dictionary:
            value = env.dictionary[self.name]

            def _evaluate_single(operand):
                return evaluate(operand, env)

            if (isinstance(value, runtime.ClosureType) or
                    isinstance(value, runtime.BuiltinClosureType)):
                num_arguments = len(value.arguments)
                num_operands = len(self.operands)
                operands = self.operands[:num_arguments]
                remaining_operands = self.operands[(num_arguments - num_operands):]
                evaluated_operands = map(_evaluate_single, operands)
                evaluated_remaining_operands = map(_evaluate_single, remaining_operands)
                if isinstance(value, runtime.ClosureType):
                    lambda_env = env.copy_with(value.env.dictionary)
                    for variable, operand in zip(value.arguments, evaluated_operands):
                        lambda_env = lambda_env.copy_with({variable: operand})
                    lambda_env = lambda_env.copy_with({value.remaining_args: remaining_operands})
                    return evaluate(value.body, lambda_env)
                elif isinstance(value, runtime.BuiltinClosureType):
                    return value.operation(evaluated_operands, evaluated_remaining_operands)
            elif isinstance(value, runtime.StructDeclarationType):
                return runtime.StructInstantiationType(value, map(_evaluate_single, self.operands))
            else:
                raise Exception("Wrong runtime %s %s" % (value, type(value)))
        else:
            raise Exception("Function declaration not found \"%s\"" % self.name)

    elif isinstance(self, ast.StructDeclarationNode):
        return runtime.StructDeclarationType(self.members)

    elif isinstance(self, ast.MemberAccessNode):
        if self.struct_name in env.dictionary:
            struct = env.dictionary[self.struct_name]
            if self.member_name in struct.values:
                return struct.values[self.member_name]
            else:
                raise Exception("Struct %s does not have member %s" % (
                    self.struct_name, self.member_name))
        else:
            raise Exception("Struct %s not found" % self.member_name)

    elif isinstance(self, ast.ConditionalNode):
        if evaluate(self.if_expr, env).val is True:
            return evaluate(self.then_expr, env)
        else:
            return evaluate(self.else_expr, env)

    elif isinstance(self, ast.NegateNode):
        evaluated_expr = evaluate(self.expr, env)
        if evaluated_expr.val is True:
            return runtime.BoolType(False)
        elif evaluated_expr.val is False:
            return runtime.BoolType(True)
        else:
            raise Exception('expected boolean, got %s' % evaluated_expr)
    elif isinstance(self, ast.GetEnvironmentBindingNode):
        return runtime.BindingType(env)

    elif isinstance(self, ast.LoadingNode):
        raw_string = ''
        with open('lib/%s.fr' % self.binding_name) as f:
            raw_string = ''.join(f.readlines())
        imported_ast = parse.Parser(raw_string).parse_expression()
        imported_val = evaluate(imported_ast, env)
        imported_env = env.copy_with(imported_val.env.dictionary)
        return evaluate(self.body, imported_env)

    else:
        raise Exception("Invalid AST Node Type %s (%s)" % (type(self), self))
