delimiters = ['(', ')', ' ']


def peek(remaining):
    current, remaining = get_token(remaining)
    print 'peeked  : %s rest : %s' % (current, remaining)
    return current


def chomp(remaining):
    current, remaining = get_token(remaining)
    print 'chomped : %s rest : %s' % (current, remaining)
    return current, remaining


def get_token(remaining):
    current, remaining = get_next_non_whitespace(remaining)
    token = current
    if not current in delimiters:
        while True:
            current, remaining = get_next_character(remaining)
            if current in delimiters:
                remaining = current + remaining
                break
            token += current
    return token, remaining


def get_next_character(remaining):
    return remaining[0], remaining[1:]


def get_next_non_whitespace(remaining):
    current, remaining = get_next_character(remaining)
    while current in [' ', '\n', '\r', '\t']:
        current, remaining = get_next_character(remaining)
    return current, remaining


def to_pieces(string):
    remaining = string
    pieces = []
    while remaining != '':
        current, remaining = chomp(remaining)
        pieces.append(current)
    return pieces
