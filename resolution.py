from knowledge_base import KnowledgeBase
from itertools import combinations
from copy import deepcopy
from sentence_engine import (
    to_cnf, extract_proposition_symbols, negate,
    smart_tokenize)


def pl_resolve(c1, c2):
    sentence = c1 + ' or ' + c2
    tokens = smart_tokenize(sentence)
    symbols = extract_proposition_symbols(sentence)

    resolvents = set()

    for s in symbols:
        temp_tokens = deepcopy(tokens)
        if s in tokens and negate(s) in tokens:
            temp_tokens.remove(s)
            temp_tokens.remove(negate(s))
            resolvents.add(temp_tokens)

    return [' or '.join(r) for r in resolvents]


def pl_resolution(kb, alpha):
    cnf_form = to_cnf("{} and not ( {} )".format(kb.as_sentence(), alpha))
    clauses = {clause for clause in cnf_form.split('and')}
    new = set()

    while True:
        for c1, c2 in combinations(clauses, 2):
            resolvents = pl_resolve(c1, c2)
            if '' in resolvents:
                return True
            new.add(resolvents)

        if new.issubset(clauses):
            return False

        clauses.update(new)


def main():
    kb = KnowledgeBase()
    kb.tell('not p11')
    kb.tell('b11 <=> ( p12 or p21 )')
    kb.tell('b21 <=> ( p11 or p22 or p31 )')
    kb.tell('not b11')
    kb.tell('b21')

    alpha = 'not p12'

    print(pl_resolution(kb, alpha))


if __name__ == '__main__':
    main()
