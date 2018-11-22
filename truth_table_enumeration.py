from knowledge_base import KnowledgeBase
from anytree import LevelOrderIter
from sentence_engine import get_expression_tree, extract_proposition_symbols
from utils import Argument
from copy import deepcopy


def is_pl_true(sentence, model):
    root = get_expression_tree(sentence)

    execution_order = [node for node in LevelOrderIter(root)]
    execution_order.reverse()

    for node in execution_order:
        if isinstance(node, Argument):
            node.value = model.get(node.symbol)
        else:
            node.calculate()

    return execution_order[-1].value


def check_all(kb, alpha, symbols, model):
    if not symbols:
        if all(is_pl_true(s, model) for s in kb.sentences):
            print('model {} satisfies kb'.format(model))
            res = is_pl_true(alpha, model)
            print('Does it satisfy alpha ? {}'.format(res))
            print()
            return res
        return True

    p = symbols.pop()

    symbols1 = deepcopy(symbols)
    model1 = deepcopy(model)
    model1[p] = True

    symbols2 = deepcopy(symbols)
    model2 = deepcopy(model)
    model2[p] = False

    return (
            check_all(kb, alpha, symbols1, model1)
            and
            check_all(kb, alpha, symbols2, model2)
    )


def check_if_entails(kb, alpha):
    symbols = extract_proposition_symbols(alpha)
    symbols.update([s for sentence in kb.sentences for s in extract_proposition_symbols(sentence)])
    return check_all(kb, alpha, symbols, {})


def main():
    kb = KnowledgeBase()
    kb.tell('not p11')
    kb.tell('b11 <=> ( p12 or p21 )')
    kb.tell('b21 <=> ( p11 or p22 or p31 )')
    kb.tell('not b11')
    kb.tell('b21')

    alpha = 'not p12'

    result = check_if_entails(kb, alpha)
    print('kb entails alpha ? {}'.format(result))


if __name__ == '__main__':
    main()
