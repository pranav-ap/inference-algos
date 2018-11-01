from knowledge_base import KnowledgeBase
from parsers import extract_preposition_symbols, get_expression_tree
from anytree import LevelOrderIter
from utils import ConnectiveType
from copy import deepcopy


def is_pl_true(sentence, model):
    root = get_expression_tree(sentence, model)

    execution_order = [node for node in LevelOrderIter(root)]
    execution_order.reverse()

    for node in execution_order:
        if node.is_leaf:
            continue

        if node.op == ConnectiveType.NOT.value:
            node.value = not node.children[0].value
        elif node.op == ConnectiveType.AND.value:
            node.value = node.children[0].value and node.children[1].value
        elif node.op == ConnectiveType.OR.value:
            node.value = node.children[0].value or node.children[1].value

    return root.value


def check_all(kb, alpha, symbols, model):
    if not symbols:
        if all(is_pl_true(s, model) for s in kb.sentences):
            print(model)
            return is_pl_true(alpha, model)
        return True

    p = symbols.pop()

    model1 = deepcopy(model)
    model1[p] = False

    model2 = deepcopy(model)
    model2[p] = True

    return (
            check_all(kb, alpha, symbols, model1)
            and
            check_all(kb, alpha, symbols, model2)
    )


def check_if_entails(kb, alpha):
    symbols = extract_preposition_symbols(alpha)
    symbols.update([s for sentence in kb.sentences for s in extract_preposition_symbols(sentence)])
    return check_all(kb, alpha, symbols, {})


def main():
    kb = KnowledgeBase()
    kb.tell('a and b or c')
    alpha = 'a and b'

    result = check_if_entails(kb, alpha)
    print('Entails ? : {}'.format(result))


if __name__ == '__main__':
    main()
