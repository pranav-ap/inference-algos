from anytree import RenderTree, findall, LevelOrderIter, PostOrderIter
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


def expression_tree_to_sentence(root):
    sentence = [node.symbol for node in PostOrderIter(root)]
    print(sentence)
    sentence.reverse()
    sentence = postfix_to_infix(sentence)

    return sentence


def postfix_to_infix(postfix):
    stack = []

    while postfix:
        p = postfix.pop()

        if p not in operators:
            stack.append(p)
        else:
            if p == 'not':
                child = stack.pop()
                stack.append('( not {} )'.format(child))
            else:
                lhs, rhs = stack.pop(), stack.pop()
                stack.append('( {} {} {} )'.format(lhs, p, rhs))

    return stack[0]


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
            node = Argument(symbol=token)

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
    deathrow = [n for n in deathrow if isinstance(n.child, And) or isinstance(n.child, Or)]

    for inmate in deathrow:
        operator = inmate.child
        operator.parent = None

        left_child, right_child = operator.children
        not_node_1 = Not(child=left_child)
        not_node_2 = Not(child=right_child)
        new_op = And(lhs=not_node_1, rhs=not_node_2) if isinstance(operator, Or) else Or(lhs=not_node_1, rhs=not_node_2)

        if inmate.is_root:
            new_op.parent = None
            root = new_op
        else:
            if inmate == inmate.parent.lhs:
                inmate.parent.lhs = new_op
            else:
                inmate.parent.rhs = new_op

            new_op.parent = inmate.parent
            inmate.parent = None

    for node in LevelOrderIter(root):
        if isinstance(node, Not) and isinstance(node.child, Not):
            node.child.parent = node.parent
            node.parent = None

    return root


def move_disjunctions_inwards(root):
    deathrow = get_death_row(root, Or)
    deathrow = [n for n in deathrow if isinstance(n.lhs, And) or isinstance(n.rhs, And)]

    for inmate in deathrow:
        child1, child2 = inmate.children
        and_operator= child1 if isinstance(child1, And) else child2
        other_child = child2 if child2 != and_operator else child1
        other_child2 = deepcopy(other_child)
        and_child1, and_child2 = and_operator.children

        or1 = Or(lhs=and_child1, rhs=other_child)
        or2 = Or(lhs=and_child2, rhs=other_child2)
        and_node = And(parent=inmate.parent, lhs=or1, rhs=or2)

        if inmate.is_root:
            and_node.parent = None
            root = and_node
        else:
            if inmate == inmate.parent.lhs:
                inmate.parent.lhs = and_node
            else:
                inmate.parent.rhs = and_node

            inmate.parent = None

    return root


def to_cnf(sentence):
    root = get_expression_tree(sentence)
    root = eliminate_bidirectional(root)
    root = eliminate_implication(root)
    root = move_not_inwards(root)
    root = move_disjunctions_inwards(root)

    print(RenderTree(root))

    sentence = expression_tree_to_sentence(root)

    return sentence


def main():
    print(to_cnf('( b11 <=> ( p12 or ( w and not p21 ) ) )'))
    # print(to_cnf('a and ( b or ( d and e ) )'))



if __name__ == '__main__':
    main()
