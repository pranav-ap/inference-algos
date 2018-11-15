from utils import extract_preposition_symbols
from knowledge_base import KnowledgeBase
from itertools import combinations


def pl_resolution(kb, alpha):
    cnf_form = get_cnf_form("{} and not {}".format(kb.as_sentence(), alpha))
    clauses = {clause for clause in cnf_form.split('and')}
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
    kb = KnowledgeBase()
    kb.tell('not p11')
    kb.tell('b11 <=> ( p12 or p21 )')
    kb.tell('b21 <=> ( p11 or p22 or p31 )')
    kb.tell('not b11')
    kb.tell('b21')

    alpha = 'not p12'

    cnf_form = get_cnf_form("{} and not ( {} )".format(kb.as_sentence(), alpha))
    print(cnf_form)


if __name__ == '__main__':
    main()
