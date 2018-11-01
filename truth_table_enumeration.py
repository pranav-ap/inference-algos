from knowledge_base import KnowledgeBase
from parsers import extract_preposition_symbols, get_expression_tree
from anytree import RenderTree, LevelOrderIter


def is_pl_true(sentence, model):
    root = get_expression_tree(sentence, model)
    print(RenderTree(root))
    execution_order = [node for node in LevelOrderIter(root)]
    execution_order.reverse()
    print(execution_order)

    stack = []


    return True


def check_all(kb, alpha, symbols, model):
    if not symbols:
        if all(is_pl_true(s, model) for s in kb.sentences):
            return is_pl_true(alpha, model)
        return True

    p = symbols.pop()

    model1 = model
    model1[p] = True

    model2 = model
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
    print('Result : {}'.format(result))


if __name__ == '__main__':
    main()
