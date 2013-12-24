whitespace = [' ', '\n', '\r', '\t']
delimiters = ['(', ')', '[', ']', ''] + whitespace


class Tokenizer:
    def __init__(self, source_string):
        self.original_source = source_string
        self.remaining = source_string
        self.current = ''
        self.last = ''

    def peek(self):
        current, remaining = _get_token(self.remaining)
        #print('peeked  : %s rest : %s' % (current, remaining))
        return current

    def chomp(self):
        self.last = self.current
        self.current, self.remaining = _get_token(self.remaining)
        #print('chomped : %s rest : %s' % (self.current, self.remaining))
        return self.current

    def to_pieces(self):
        def _get_piece(remaining, pieces):
            if remaining == '':
                return pieces
            else:
                self.chomp()
                return _get_piece(self.remaining, pieces + [self.current])
        return _get_piece(self.original_source, [])


def _get_token(remaining):
    def _get_next_character(remaining):
        if len(remaining) > 1:
            return remaining[0], remaining[1:]
        elif len(remaining) > 0:
            return remaining[0], ''
        else:
            return '', ''

    def _get_next_non_whitespace(remaining):
        current, remaining = _get_next_character(remaining)
        while current in whitespace:
            current, remaining = _get_next_character(remaining)
        return current, remaining

    current, remaining = _get_next_non_whitespace(remaining)
    token = current
    if not current in delimiters:
        while True:
            current, remaining = _get_next_character(remaining)
            if current in delimiters:
                remaining = current + remaining
                break
            token += current
    return token, remaining
