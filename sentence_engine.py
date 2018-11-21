from anytree import RenderTree
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


def assemble(root):
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


def get_as_class(token):
    if token == 'not':
        return Not()
    elif token == 'or':
        return Or()
    elif token == 'and':
        return And()
    elif token == '=>':
        return Implies()
    elif token == '<=>':
        return Bidirectional()
    elif token in ['(', ')']:
        return token
    else:
        return Argument(value=token)


def get_expression_tree(sentence):
    postfix = infix_to_postfix(sentence)
    postfix = dumb_tokenize(postfix)

    root = get_as_class(postfix[-1])
    stack = []

    for token in postfix:
        node = None

        if token == 'not':
            child = stack.pop()
            node = Not(child=child)
            child.parent = node
        elif token in ['or', 'and', '=>', '<=>']:
            lhs, rhs = stack.pop(), stack.pop()

            if token == 'or':
                node = Or(lhs=lhs, rhs=rhs)
            elif token == 'and':
                node = And(lhs=lhs, rhs=rhs)
            elif token == '=>':
                node = Implies(lhs=lhs, rhs=rhs)
            elif token == '<=>':
                node = Bidirectional(lhs=lhs, rhs=rhs)

            lhs.parent = rhs.parent = node
        else:
            node = Argument(value=token)

        stack.append(node)

    return root


def main():
    print(RenderTree(get_expression_tree('( b11 <=> ( p12 or not p21 ) )')))


if __name__ == '__main__':
    main()
