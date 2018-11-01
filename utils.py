from enum import Enum
from anytree import NodeMixin


class ValencyType(Enum):
    NULL = 0
    UNARY = 1
    BINARY = 2


class ConnectiveType(Enum):
    NULL = ''
    AND = 'and'
    OR = 'or'
    NOT = 'not'
    IMPLIES = '=>'
    BIDIRECTIONAL = '<=>'


connectives = [x[1].value for x in ConnectiveType.__members__.items()]


logical_precedence = {
    ConnectiveType.NOT.value: 5,
    ConnectiveType.AND.value: 4,
    ConnectiveType.OR.value: 3,
    ConnectiveType.IMPLIES.value: 2,
    ConnectiveType.BIDIRECTIONAL.value: 1
}


class Operator(NodeMixin):
    def __init__(self, op, parent=None):
        self.op = str(op)
        self.parent = parent

    def get_type(self):
        return ConnectiveType(self.op)

    def __repr__(self):
        return 'Operator : {}'.format(self.op)


# Represents a single value argument
class Argument(NodeMixin):
    def __init__(self, arg, value=None, parent=None):
        self.arg = arg
        self.value = value
        self.parent = parent

    def get_type(self):
        return 'hi'

    def __repr__(self):
        return 'Argument : {}'.format(self.arg)