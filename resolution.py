from utils import extract_preposition_symbols
from itertools import combinations


def get_cnf_form(sentence):
    clauses = [clause.trim() for clause in sentence.split('and')]

    for clause in clauses:
        if '<=>' in clause:
            # Eliminate <=>
            pass
        if '=>' in clause:
            # Eliminate =>
            pass
        if 'not (' in clause:
            # Move not inwards
            pass
        if 'and' in clause:
            # Apply distribution law
            pass
    
    return


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
    resolvents = pl_resolve('not p21 or b11', 'not b11 or p12 or p21')
    # resolvents = pl_resolve('not p12 or b11', 'not b11')
    print(resolvents)


if __name__ == '__main__':
    main()
