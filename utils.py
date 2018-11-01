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


# Represents a single value argument
class Argument(NodeMixin):
    def __init__(self, arg, parent=None):
        self.arg = arg
        self.parent = parent
