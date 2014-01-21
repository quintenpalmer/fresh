import json


def get_correctness_message(boolean):
    if boolean:
        return "was"
    else:
        return "wasn't"


def pre_pad(message):
    return ' '*(6-len(str(message))) + str(message)


def format_message(result, expected, original):
    return '%s should be %s and it %s source: %s' % (
        pre_pad(result),
        pre_pad(expected),
        get_correctness_message(result == expected),
        original)


class TestExpression:
    def __init__(self, expr, evaluation, tokenized):
        self.expr = expr
        self.evaluation = evaluation
        self.tokenized = tokenized


def get_cases():
    with open('test/examples.json', 'r') as f:
        test_cases = []
        test_cases_data = json.load(f)
        for test_case in test_cases_data:
            test_cases.append(
                TestExpression(
                    test_case['expr'],
                    test_case['evaluation'],
                    test_case['tokenized']))
        return test_cases
