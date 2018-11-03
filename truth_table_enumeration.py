from knowledge_base import KnowledgeBase
from parsers import extract_preposition_symbols, get_expression_tree
from anytree import LevelOrderIter
from utils import OperatorType
from copy import deepcopy


def is_pl_true(sentence, model):
    root = get_expression_tree(sentence)

    execution_order = [node for node in LevelOrderIter(root)]
    execution_order.reverse()

    for node in execution_order:
        if node.is_leaf:
            node.value = model.get(node.arg)
            continue

        if node.op == OperatorType.NOT.value:
            node.value = not node.children[0].value
        elif node.op == OperatorType.AND.value:
            node.value = node.children[0].value and node.children[1].value
        elif node.op == OperatorType.OR.value:
            node.value = node.children[0].value or node.children[1].value
        elif node.op == OperatorType.IMPLIES.value:
            node.value = not node.children[0].value or node.children[1].value
        elif node.op == OperatorType.BIDIRECTIONAL.value:
            node.value = node.children[0].value == node.children[1].value

    return root.value


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
    branch1 = deepcopy(model)
    branch1[p] = True

    symbols2 = deepcopy(symbols)
    branch2 = deepcopy(model)
    branch2[p] = False

    return (
            check_all(kb, alpha, symbols1, branch1)
            and
            check_all(kb, alpha, symbols2, branch2)
    )


def check_if_entails(kb, alpha):
    symbols = extract_preposition_symbols(alpha)
    symbols.update([s for sentence in kb.sentences for s in extract_preposition_symbols(sentence)])
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
