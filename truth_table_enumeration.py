from knowledge_base import KnowledgeBase
from parsers import extract_preposition_symbols
from utils import Operator, Argument, ConnectiveType


def is_pl_true(node, model):
    if isinstance(node, Argument):
        return node.value

    if node.op == ConnectiveType.NOT.value:
        p = is_pl_true(node.children[0], model)
        if p is None:
            return None
        else:
            return not p
    elif node.op == ConnectiveType.OR.value:
        result = False
        for arg in args:
            p = pl_true(arg, model)
            if p is True:
                return True
            if p is None:
                result = None
        return result
    elif node.op == ConnectiveType.AND.value:
        result = True
        for arg in args:
            p = pl_true(arg, model)
            if p is False:
                return False
            if p is None:
                result = None
        return result

    p, q = args
    if op == '==>':
        return pl_true(~p | q, model)
    elif op == '<==':
        return pl_true(p | ~q, model)
    pt = pl_true(p, model)
    if pt is None:
        return None
    qt = pl_true(q, model)
    if qt is None:
        return None
    if op == '<=>':
        return pt == qt
    elif op == '^':  # xor or 'not equivalent'
        return pt != qt
    else:
        raise ValueError("illegal operator in logic expression" + str(node))


def check_if_entails(kb, alpha):
    symbols = extract_preposition_symbols(alpha)
    symbols.extend([s for sentence in kb.sentences for s in extract_preposition_symbols(sentence)])
    model = {}
    return check_all(kb, alpha, symbols, model)


def check_all(kb, alpha, symbols, model):
    if not symbols:
        if all(is_pl_true(s, model) for s in kb.sentences):
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
