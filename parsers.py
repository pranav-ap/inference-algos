from utils import (
    Operator, Argument, logical_precedence,
    operators, OperatorType
    )
from anytree import RenderTree


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

    return output


def get_expression_tree(sentence):
    stack = []
    postfix = infix_to_postfix(sentence)
    postfix = map(lambda arg: Operator(arg) if arg in operators else Argument(arg), postfix)
    postfix = list(postfix)

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
    print(RenderTree(root))
    return root


def main():
    # postfix = infix_to_postfix('a and b => c')
    postfix = get_expression_tree('( b11 <=> ( p12 or p21 ) )')
    print(postfix)


if __name__ == '__main__':
    main()

