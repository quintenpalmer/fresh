import tokenizer
import test_examples

for example in test_examples.examples:
    tokenized = tokenizer.to_pieces(example.expr)
    print tokenized == example.tokenized, tokenized, example.tokenized, example.expr
    if tokenized != example.tokenized:
        raise Exception('BAD BAD BAD BAD')
print 'OK'
