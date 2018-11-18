from anytree import NodeMixin


operators = ['not', 'and', 'or', '(', ')', '=>', '<=>']


class Operator(NodeMixin):
    def __init__(self, parent=None):
        self.parent = parent
        self.computed_value = None

    def calculate(self):
        raise NotImplementedError()


class UnaryOperator(Operator):
    def __init__(self, parent=None, child=None):
        Operator.__init__(self, parent=parent)
        self.child = child

    def calculate(self):
        raise NotImplementedError()


class Not(UnaryOperator):
    def __init__(self, parent=None, child=None):
        UnaryOperator.__init__(self, parent=parent, child=child)

    def calculate(self):
        self.computed_value = not self.child.value


class BinaryOperator(Operator):
    def __init__(self, parent=None, lhs=None, rhs=None):
        Operator.__init__(self, parent=parent)
        self.lhs = lhs
        self.rhs = rhs

    def calculate(self):
        raise NotImplementedError()


class And(BinaryOperator):
    def __init__(self, parent=None, lhs=None, rhs=None):
        BinaryOperator.__init__(self, parent=parent, lhs=lhs, rhs=rhs)

    def calculate(self):
        self.computed_value = self.lhs.value and self.rhs.value


class Or(BinaryOperator):
    def __init__(self, parent=None, lhs=None, rhs=None):
        BinaryOperator.__init__(self, parent=parent, lhs=lhs, rhs=rhs)

    def calculate(self):
        self.computed_value = self.lhs.value or self.rhs.value


class Implies(BinaryOperator):
    def __init__(self, parent=None, lhs=None, rhs=None):
        BinaryOperator.__init__(self, parent=parent, lhs=lhs, rhs=rhs)

    def calculate(self):
        self.computed_value = False if self.lhs.value and not self.rhs.value else True


class Bidirectional(BinaryOperator):
    def __init__(self, parent=None, lhs=None, rhs=None):
        BinaryOperator.__init__(self, parent=parent, lhs=lhs, rhs=rhs)

    def calculate(self):
        self.computed_value = self.lhs.value == self.rhs.value


class Argument(NodeMixin):
    def __init__(self, parent=None, value=None):
        self.parent = parent
        self.value = value


def main():
    pass


if __name__ == '__main__':
    main()
