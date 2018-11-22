from anytree import RenderTree, findall
from copy import deepcopy
from utils import (
    Argument, logical_precedence, operators,
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
    return


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

    return root


def eliminate_implication(root):
    pass


def move_not_inwards(root):
    pass


def distribute_and_over_or(root):
    pass


def to_cnf(root):
    root = eliminate_bidirectional(root)
    # root = eliminate_implication(root)
    # root = move_not_inwards(root)
    # root = distribute_and_over_or(root)

    return root


def main():
    print(RenderTree(to_cnf(get_expression_tree('( b11 <=> ( p12 or not p21 ) )'))))


if __name__ == '__main__':
    main()
