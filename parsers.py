from anytree import NodeMixin
from utils import ConnectiveType

def extract_prepositional_symbols(sentence):
    connectives = [x[1].value for x in ConnectiveType.__members__.items()]

    symbols = [s.lower() for s in sentence.split(' ')]
    symbols = [s for s in symbols if s not in connectives]
    return symbols


class Operator(NodeMixin):
    def __init__(self, op, parent=None):
        self.op = str(op)
        self.parent = parent


# Represents a single value argument
class Argument(NodeMixin):
    def __init__(self, arg, parent=None):
        self.arg = arg
        self.parent = parent


# Represents an operator along with its arguments (single value argument or expressions)
class Expr:
    def __init__(self, op, *args):
        self.op = Operator(op, parent=None)

        for arg in args:
            if isinstance(arg, Expr):
                arg.parent = self
            else:
                child = Argument(arg, parent=op)
                child.parent = self.op


def main():
    print(extract_prepositional_symbols('a and b => c'))
    print(extract_prepositional_symbols('a and b or c'))


if __name__ == '__main__':
    main()

