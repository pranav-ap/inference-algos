from utils import extract_preposition_symbols
from knowledge_base import KnowledgeBase
from itertools import combinations


def remove_dangling_connectives(sentence):
    # remove extra whitespaces
    sentence = ' '.join(sentence.split())
    sentence = sentence.replace('or or or', 'or')
    sentence = sentence.replace('or or', 'or')
    sentence = ' '.join(sentence.split())

    if sentence.startswith('and') or sentence.startswith('or') :
        sentence = sentence.split(' ', 1)[1]
    if sentence.endswith('and') or sentence.endswith('or') :
        sentence = sentence.rsplit(' ', 1)[0]

    return sentence


def both_in_sentence(neg_symbol, pos_symbol, sentence):
    neg_symbol_exists = neg_symbol in sentence
    pos_symbol_exists = False
    
    if sentence.startswith(pos_symbol):
        pos_symbol_exists = True
    else:
        sentence = sentence.split(' ')

        for previous, symbol in zip(sentence, sentence[1:]):
            if symbol == pos_symbol and previous != 'not':
                pos_symbol_exists = True
    
    return neg_symbol_exists and pos_symbol_exists    


def pl_resolve(c1, c2):
    resolvents = set()
    sentence = c1 + ' or ' + c2
    symbols = extract_preposition_symbols(sentence)

    for symbol in symbols:
        neg_symbol = 'not {}'.format(symbol)
        if both_in_sentence(neg_symbol, symbol, sentence):
            resolvent = sentence

            resolvent = resolvent.replace(neg_symbol, '')
            resolvent = resolvent.replace(symbol, '')
            
            resolvent = remove_dangling_connectives(resolvent)
            resolvents.add(resolvent)
    
    return resolvents


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
    # resolvents = pl_resolve('not p21 or b11', 'not b11 or p12 or p21')
    # resolvents = pl_resolve('not p12 or b11', 'not b11')
    # print(resolvents)

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
