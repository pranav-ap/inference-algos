from utils import Operator, Argument, logical_precedence, operators, OperatorType
from anytree import RenderTree, findall
from copy import deepcopy


def prepare(sentence):
    # remove extra whitespaces
    sentence = ' '.join(sentence.split(' '))
    return sentence


def tokenize(sentence):
    previous = ''
    tokens = []

    for current in sentence.split(' ') + ['$']:
        if previous == '':
            previous = current
            continue

        if previous != OperatorType.NOT.value:
            tokens.append(previous)
            previous = current
        elif current not in operators:
            tokens.append(previous + ' ' + current)
            previous = ''
        else:
            tokens.append(previous)
            previous = current

    return tokens


def negate(symbol):
    return '{} {}'.format(OperatorType.NOT.value, symbol) if not symbol.startswith(OperatorType.NOT.value) else symbol


def extract_proposition_symbols(sentence):
    symbols = {s for s in tokenize(sentence)}
    symbols = {s for s in symbols if s not in operators}
    return symbols


def complement_exists(pos_symbol, sentence):
    neg_symbol = negate(pos_symbol)
    tokens = tokenize(sentence)
    return pos_symbol in tokens and neg_symbol in tokens


def is_disjunction(sentence):
    return not sentence.startswith('not (') and all(token not in ['and', '=>', '<=>'] for token in sentence)


def is_conjunction(sentence):
    return not sentence.startswith('not (') and all(token not in ['or', '=>', '<=>'] for token in sentence)


def infix_to_postfix(sentence):
    stack = []
    output = []

    for token in sentence.split(' '):
        if token not in operators:
            output.append(token)
        elif token == OperatorType.LEFT_PARENTHESIS.value:
            stack.append(token)
        elif token == OperatorType.RIGHT_PARENTHESIS.value:
            # pop until a '(' is popped
            while stack and stack[-1] != OperatorType.LEFT_PARENTHESIS.value:
                output.append(stack.pop())
            stack.pop()
        else:
            while stack and stack[-1] != OperatorType.LEFT_PARENTHESIS.value and logical_precedence[token] <= logical_precedence[stack[-1]]:
                output.append(stack.pop())
            stack.append(token)

    # leftover
    while stack:
        output.append(stack.pop())

    return ' '.join(output)


def get_expression_tree(sentence):
    stack = []
    postfix = infix_to_postfix(sentence).split(' ')
    postfix = list(map(lambda arg: Operator(arg) if arg in operators else Argument(arg), postfix))

    root = postfix[-1]

    for node in postfix:
        if isinstance(node, Argument):
            stack.append(node)
        else:
            if node.op == OperatorType.NOT.value:
                arg = stack.pop()
                arg.parent = node
            else:
                arg1 = stack.pop()
                arg1.parent = node
                arg2 = stack.pop()
                arg2.parent = node

            stack.append(node)

    return root

# Conjunctive Normal Form Conversion

def _eliminate_bidirection(self, root):
    if not (isinstance(root, Operator) and root.op == '<=>'):
        return root

    and_node = Operator('and')
    left_implication = Operator('=>', parent=and_node)
    right_implication = Operator('=>', parent=and_node)

    left_implication_left, left_implication_right = root.children
    right_implication_right, right_implication_left = deepcopy(left_implication_left), deepcopy(
        left_implication_right)

    left_implication_left.parent = left_implication
    left_implication_right.parent = left_implication

    right_implication_left.parent = right_implication
    right_implication_right.parent = right_implication

    return and_node


def _eliminate_implication(self, root):
    if not (isinstance(root, Operator) and root.op == '=>'):
        return root

    or_node = Operator('or')
    not_node = Operator('not')
    not_node.parent = or_node

    left, right = root.children
    left.parent = not_node
    right.parent = or_node

    return or_node


def _move_not_inwards(self, root):
    deathrow = findall(
        root,
        filter_=lambda node: isinstance(node, Operator) and node.op == 'not' and isinstance(node.children[0],
                                                                                            Operator))

    for inmate in deathrow:
        before = inmate.parent
        after = inmate.children[0]
        after.op = 'or' if after.op == 'and' else 'and'
        after.parent = before
        inmate.parent = None

        for child in after.children:
            if isinstance(child, Operator) and child.op == 'not':
                temp = child.children[0]
                child.parent = None
                child = temp
                child.parent = after
            else:
                not_node = Operator('not')
                not_node.parent = after
                child.parent = not_node

    return root


def _apply_distribution_law(self, root):
    deathrow = findall(
        root,
        filter_=lambda node: isinstance(node, Operator) and node.op == 'or'
                             and any(isinstance(n, Operator) and n.op == 'and' for n in node.children))

    print(deathrow)

    return root


def to_conjunctive_normal_form(sentence):
    root = get_expression_tree(sentence)

    print(RenderTree(root))
    root = self._eliminate_bidirection(root)
    print(RenderTree(root))
    root = self._eliminate_implication(root)
    print(RenderTree(root))
    root = self._move_not_inwards(root)
    print(RenderTree(root))
    root = self._apply_distribution_law(root)

    return root


def main():
    print(RenderTree(to_conjunctive_normal_form('( b11 <=> ( p12 or not p21 ) )')))


if __name__ == '__main__':
    main()