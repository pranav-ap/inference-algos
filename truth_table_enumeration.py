from knowledge_base import KnowledgeBase
from parsers import extract_prepositional_symbols
from utils import pl_true


def check_if_entails(kb, alpha):
    symbols = extract_prepositional_symbols(kb.as_sentence()) + extract_prepositional_symbols(alpha)
    model = {}
    return check_all(kb, alpha, symbols, model)


def check_all(kb, alpha, symbols, model):
    if not symbols:
        if pl_true(kb, model):
            return pl_true(alpha, model)
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
