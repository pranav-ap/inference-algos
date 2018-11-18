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
        self.computed_value = None

    def calculate(self):
        raise NotImplementedError()

    def __repr__(self):
        return 'Operator'


class UnaryOperator(Operator):
    def __init__(self, parent=None, child=None):
        Operator.__init__(self, parent=parent)
        self.child = child

    def calculate(self):
        raise NotImplementedError()

    def __repr__(self):
        return 'UnaryOperator'


class Not(UnaryOperator):
    def __init__(self, parent=None, child=None):
        UnaryOperator.__init__(self, parent=parent, child=child)

    def calculate(self):
        self.computed_value = not self.child.value

    def __repr__(self):
        return 'Not'


class BinaryOperator(Operator):
    def __init__(self, parent=None, lhs=None, rhs=None):
        Operator.__init__(self, parent=parent)
        self.lhs = lhs
        self.rhs = rhs

    def calculate(self):
        raise NotImplementedError()

    def __repr__(self):
        return 'BinaryOperator'


class And(BinaryOperator):
    def __init__(self, parent=None, lhs=None, rhs=None):
        BinaryOperator.__init__(self, parent=parent, lhs=lhs, rhs=rhs)

    def calculate(self):
        self.computed_value = self.lhs.value and self.rhs.value

    def __repr__(self):
        return 'And'


class Or(BinaryOperator):
    def __init__(self, parent=None, lhs=None, rhs=None):
        BinaryOperator.__init__(self, parent=parent, lhs=lhs, rhs=rhs)

    def calculate(self):
        self.computed_value = self.lhs.value or self.rhs.value

    def __repr__(self):
        return 'Or'


class Implies(BinaryOperator):
    def __init__(self, parent=None, lhs=None, rhs=None):
        BinaryOperator.__init__(self, parent=parent, lhs=lhs, rhs=rhs)

    def calculate(self):
        self.computed_value = False if self.lhs.value and not self.rhs.value else True

    def __repr__(self):
        return 'Implies'


class Bidirectional(BinaryOperator):
    def __init__(self, parent=None, lhs=None, rhs=None):
        BinaryOperator.__init__(self, parent=parent, lhs=lhs, rhs=rhs)

    def calculate(self):
        self.computed_value = self.lhs.value == self.rhs.value

    def __repr__(self):
        return 'Bidirectional'


class Argument(NodeMixin):
    def __init__(self, parent=None, value=None):
        self.parent = parent
        self.value = value

    def __repr__(self):
        return 'Argument {}'.format(self.value)


def main():
    pass


if __name__ == '__main__':
    main()
