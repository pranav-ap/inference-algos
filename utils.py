from anytree import NodeMixin


operators = ['not', 'and', 'or', '(', ')', '=>', '<=>']

logical_precedence = {
    'not': 7,
    'and': 6,
    'or': 5,
    '(': 4,
    ')': 3,
    '=>': 2,
    '<=>': 1
}


class Operator(NodeMixin):
    def __init__(self, parent=None):
        self.parent = parent
        self.value = None

    def calculate(self):
        raise NotImplementedError()

    def __repr__(self):
        return 'Operator'


class UnaryOperator(Operator):
    def __init__(self, child, parent=None):
        Operator.__init__(self, parent=parent)
        child.parent = self
        self.child = child

    def calculate(self):
        raise NotImplementedError()

    def __repr__(self):
        return 'UnaryOperator'


class Not(UnaryOperator):
    def __init__(self, child, parent=None):
        UnaryOperator.__init__(self, child, parent=parent)

    def calculate(self):
        self.value = not self.child.value

    def __repr__(self):
        return 'not {}'.format(self.child.value)


class BinaryOperator(Operator):
    def __init__(self, lhs, rhs, parent=None):
        Operator.__init__(self, parent=parent)
        lhs.parent = rhs.parent = self
        self.lhs = lhs
        self.rhs = rhs

    def calculate(self):
        raise NotImplementedError()

    def __repr__(self):
        return 'BinaryOperator'


class And(BinaryOperator):
    def __init__(self, lhs, rhs, parent=None):
        BinaryOperator.__init__(self, lhs, rhs, parent=parent)

    def calculate(self):
        self.value = self.lhs.value and self.rhs.value

    def __repr__(self):
        return '{} and {}'.format(self.lhs.value, self.rhs.value)


class Or(BinaryOperator):
    def __init__(self, lhs, rhs, parent=None):
        BinaryOperator.__init__(self, lhs, rhs, parent=parent)

    def calculate(self):
        self.value = self.lhs.value or self.rhs.value

    def __repr__(self):
        return '{} or {}'.format(self.lhs.value, self.rhs.value)


class Implies(BinaryOperator):
    def __init__(self, lhs, rhs, parent=None):
        BinaryOperator.__init__(self, lhs, rhs, parent=parent)

    def calculate(self):
        self.value = False if self.lhs.value and not self.rhs.value else True

    def __repr__(self):
        return '{} => {}'.format(self.lhs.value, self.rhs.value)


class Bidirectional(BinaryOperator):
    def __init__(self, lhs, rhs, parent=None):
        BinaryOperator.__init__(self, lhs, rhs, parent=parent)

    def calculate(self):
        self.value = self.lhs.value == self.rhs.value

    def __repr__(self):
        return '{} <=> {}'.format(self.lhs.value, self.rhs.value)


class Argument(NodeMixin):
    def __init__(self, parent=None, symbol=None, value=None):
        self.parent = parent
        self.symbol = symbol
        self.value = value

    def __repr__(self):
        return 'Argument {}'.format(self.value)


def main():
    pass


if __name__ == '__main__':
    main()
