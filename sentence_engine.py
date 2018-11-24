from anytree import RenderTree, findall, LevelOrderIter
from copy import deepcopy
from utils import (
    Operator, Argument, logical_precedence, operators,
    Not, And, Or, Implies, Bidirectional
)


def prepare(sentence):
    # remove extra whitespaces
    sentence = ' '.join(sentence.split(' '))
    return sentence


def dumb_tokenize(sentence):
    return prepare(sentence).split(' ')


def smart_tokenize(sentence):
    previous = ''
    tokens = []

    for current in dumb_tokenize(sentence) + ['$']:
        if previous == '':
            previous = current
            continue

        if previous != 'not':
            tokens.append(previous)
            previous = current
        elif current not in operators:
            tokens.append(previous + ' ' + current)
            previous = ''
        else:
            tokens.append(previous)
            previous = current

    return tokens


def negate(word):
    return word if word.startswith('not') else 'not {}'.format(word)


def extract_proposition_symbols(sentence):
    return {s for s in dumb_tokenize(sentence) if s not in operators}


def complement_exists(pos_symbol, sentence):
    neg_symbol = negate(pos_symbol)
    tokens = smart_tokenize(sentence)
    return pos_symbol in tokens and neg_symbol in tokens


def is_disjunction(sentence):
    return not sentence.startswith('not (') and all(token not in ['and', '=>', '<=>'] for token in sentence)


def is_conjunction(sentence):
    return not sentence.startswith('not (') and all(token not in ['or', '=>', '<=>'] for token in sentence)


def expression_tree_to_postfix(root):
    postfix= ''

    return postfix


def infix_to_postfix(sentence):
    stack = []
    output = []

    for token in dumb_tokenize(sentence):
        if token not in operators:
            output.append(token)
        elif token == '(':
            stack.append(token)
        elif token == ')':
            # pop until a '(' is popped
            while stack and stack[-1] != '(':
                output.append(stack.pop())
            stack.pop()
        else:
            while stack and stack[-1] != '(' and logical_precedence[token] <= logical_precedence[stack[-1]]:
                output.append(stack.pop())
            stack.append(token)

    # leftover
    while stack:
        output.append(stack.pop())

    return ' '.join(output)


def get_expression_tree(sentence):
    postfix = infix_to_postfix(sentence)
    postfix = dumb_tokenize(postfix)

    root = node = None
    stack = []

    for token in postfix:
        if token == 'not':
            child = stack.pop()
            node = Not(child=child)
        elif token in ['or', 'and', '=>', '<=>']:
            rhs, lhs = stack.pop(), stack.pop()

            if token == 'or':
                node = Or(lhs=lhs, rhs=rhs)
            elif token == 'and':
                node = And(lhs=lhs, rhs=rhs)
            elif token == '=>':
                node = Implies(lhs=lhs, rhs=rhs)
            elif token == '<=>':
                node = Bidirectional(lhs=lhs, rhs=rhs)
        else:
            node = Argument(value=token)

        stack.append(node)
        root = stack[0] if stack[0] else root

    return root


def get_death_row(root, type):
    return findall(root, filter_=lambda node: isinstance(node, type))


def eliminate_bidirectional(root):
    deathrow = get_death_row(root, Bidirectional)

    for inmate in deathrow:
        child_1, child_2 = deepcopy(inmate.children)
        left_implication = Implies(lhs=child_1, rhs=child_2)
        
        child_1, child_2 = deepcopy(inmate.children)
        right_implication = Implies(lhs=child_2, rhs=child_1)

        and_node = And(lhs=left_implication, rhs=right_implication)

        if inmate.is_root:
            and_node.parent = None 
            root = and_node
        else:
            if inmate == inmate.parent.lhs:
                inmate.parent.lhs = and_node
            else:
                inmate.parent.rhs = and_node
            
            and_node.parent = inmate.parent
            inmate.parent = None

    return root


def eliminate_implication(root):
    deathrow = get_death_row(root, Implies)

    for inmate in deathrow:
        left_child, right_child = inmate.children

        not_node = Not(child=left_child)
        or_node = Or(lhs=not_node, rhs=right_child)
        or_node.parent = inmate.parent

        if inmate.is_root:
            or_node.parent = None
            root = or_node
        else:
            if inmate == inmate.parent.lhs:
                inmate.parent.lhs = or_node
            else:
                inmate.parent.rhs = or_node

            or_node.parent = inmate.parent
            inmate.parent = None

    return root


def move_not_inwards(root):
    deathrow = get_death_row(root, Not)
    deathrow = [n for n in deathrow if isinstance(n.child, Operator)]

    for inmate in deathrow:
        operator = inmate.child

        if not isinstance(operator, Not):
            left_child, right_child = operator.children
            not_node_1 = Not(parent=operator, child=left_child)
            not_node_2 = Not(parent=operator, child=right_child)

        if inmate.is_root:
            operator.parent = None
            root = operator
        else:
            if inmate == inmate.parent.lhs:
                inmate.parent.lhs = operator
            else:
                inmate.parent.rhs = operator

            operator.parent = inmate.parent
            inmate.parent = None

    for node in LevelOrderIter(root):
        if isinstance(node, Not) and isinstance(node.child, Not):
            node.child.parent = node.parent
            node.parent = None

    return root


def distribute_and_over_or(root):
    pass


def to_cnf(root):
    root = eliminate_bidirectional(root)
    root = eliminate_implication(root)
    root = move_not_inwards(root)
    print(RenderTree(root))
    # root = distribute_and_over_or(root)

    return root


def main():
    print(RenderTree(to_cnf(get_expression_tree('( b11 <=> ( p12 and not p21 ) )'))))


if __name__ == '__main__':
    main()
