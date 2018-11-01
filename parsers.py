from utils import ConnectiveType, logical_precedence


def extract_preposition_symbols(sentence):
    connectives = [x[1].value for x in ConnectiveType.__members__.items()]

    symbols = [s.lower() for s in sentence.split(' ')]
    symbols = [s for s in symbols if s not in connectives]
    return symbols


def infix_to_postfix(sentence):
    stack = []
    output = []

    # operators are arranged according to decreasing priority
    connectives = [x[1].value for x in ConnectiveType.__members__.items()]

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


def main():
    print(infix_to_postfix('not A and not B or C'))


if __name__ == '__main__':
    main()

