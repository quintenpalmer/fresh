class _EOF:
    def __repr__(self):
        return "#EOF#"

    def __len__(self):
        return 0

EOF = _EOF()
whitespace = [' ', '\n', '\r', '\t']
delimiters = ['(', ')', '[', ']', EOF] + whitespace


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
            if remaining == EOF:
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
            return remaining[0], EOF
        else:
            return EOF, EOF

    def _get_next_non_whitespace(remaining):
        current, remaining = _get_next_character(remaining)
        while current in whitespace:
            current, remaining = _get_next_character(remaining)
        return current, remaining

    current, remaining = _get_next_non_whitespace(remaining)
    token = current
    if not current in delimiters:
        while True:
            current, will_be_remaining = _get_next_character(remaining)
            if current in delimiters:
                break
            remaining = will_be_remaining
            token += current
    return token, remaining
