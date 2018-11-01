from knowledge_base import KnowledgeBase
from parsers import extract_preposition_symbols


def is_pl_true(exp, model):
    if exp in (True, False):
        return exp


def check_if_entails(kb, alpha):
    symbols = (extract_preposition_symbols(kb.as_sentence())
               + extract_preposition_symbols(alpha))
    model = {}
    return check_all(kb, alpha, symbols, model)


def check_all(kb, alpha, symbols, model):
    if not symbols:
        if is_pl_true(kb, model):
            return is_pl_true(alpha, model)
        return True

    p = symbols.pop()

    return (
            check_all(kb, alpha, symbols, model.update({p: True}))
            and
            check_all(kb, alpha, symbols, model.update({p: False}))
    )


def main():
    kb = KnowledgeBase()
    alpha = ''

    result = check_if_entails(kb, alpha)
    print('Result : {}'.format(result))


if __name__ == '__main__':
    main()
