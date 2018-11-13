from utils import extract_preposition_symbols
from itertools import combinations


def get_cnf_form(sentence):
    pass


def pl_resolve(c1, c2):
    sentence = c1 + ' and ' + c2
    symbols = extract_preposition_symbols(sentence)
    resolvents = set()

    for symbol in symbols:
        neg_symbol = 'not {}'.format(symbol)
        if neg_symbol in sentence:
            sentence = sentence.replace(neg_symbol, '')
            sentence = sentence.replace(symbol, '')
            # todo remove dangling connectives
            resolvents.add(sentence)
    
    return resolvents


def pl_resolution(kb, alpha):
    cnf_form = get_cnf_form("{} and not {}".format(kb.as_sentence(), alpha))
    clauses = {clause.trim() for clause in cnf_form.split('and')}
    new = set()

    while True:
        for c1, c2 in combinations(clauses, 2):
            resolvents = pl_resolve(c1, c2)
            if {} in resolvents:
                return True
        if new.issubset(clauses):
            return False
        clauses.update(new)


def main():
    resolvents = pl_resolve('a or not b', 'a or b')
    print(resolvents)


if __name__ == '__main__':
    main()