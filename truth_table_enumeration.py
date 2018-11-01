from knowledge_base import KnowledgeBase
from parsers import tokens

from utils import pl_true


def get_prop_symbols(sentence):
    symbols = [x for x in sentence if x not in tokens]
    return symbols


def check_if_entails(kb, alpha):
    symbols = get_prop_symbols(kb.as_sentence()) + get_prop_symbols(alpha)
    model = {}
    return check_all(kb, alpha, symbols, model)


def check_all(kb, alpha, symbols, model):
    if not symbols:
        if pl_true(kb, model):
            return pl_true(alpha, model)
        return True

    else:
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
