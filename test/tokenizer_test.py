import test_examples

from src import tokenize

for example in test_examples.examples:
    my_tokenizer = tokenize.Tokenizer(example.expr)
    tokenized = my_tokenizer.to_pieces()
    print(tokenized == example.tokenized, tokenized, example.tokenized, example.expr)
    if tokenized != example.tokenized:
        raise Exception('BAD BAD BAD BAD')
print('OK')
