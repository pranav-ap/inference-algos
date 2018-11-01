from utils import Operator, Argument, logical_precedence, connectives, ConnectiveType
from anytree import RenderTree


def extract_preposition_symbols(sentence):
    symbols = [s.lower() for s in sentence.split(' ')]
    symbols = [s for s in symbols if s not in connectives]
    return symbols


def infix_to_postfix(sentence):
    stack = []
    output = []

    for token in sentence.split(' '):
        if token not in connectives:
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

    return output


def get_expression_tree(sentence, truth_values):
    stack = []
    postfix = infix_to_postfix(sentence)
    postfix = map(lambda arg: Operator(arg) if arg in connectives else Argument(arg), postfix)
    postfix = list(postfix)

    root = postfix[-1]

    for node in postfix:
        if isinstance(node, Argument):
            node.value = truth_values[node.arg]
            stack.append(node)
        else:
            if node.op == ConnectiveType.NOT.value:
                arg = stack.pop()
                arg.parent = node
            else:
                arg1 = stack.pop()
                arg1.parent = node
                arg2 = stack.pop()
                arg2.parent = node

            stack.append(node)

    return root


def main():
    pass


if __name__ == '__main__':
    main()

