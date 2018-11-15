from enum import Enum
from anytree import NodeMixin


class ValencyType(Enum):
    NULL = 0
    UNARY = 1
    BINARY = 2


class OperatorType(Enum):
    NULL = ''
    AND = 'and'
    OR = 'or'
    NOT = 'not'
    IMPLIES = '=>'
    BIDIRECTIONAL = '<=>'
    LEFT_PARENTHESIS = '('
    RIGHT_PARENTHESIS = ')'


operators = [x[1].value for x in OperatorType.__members__.items()]


logical_precedence = {
    OperatorType.NOT.value: 7,
    OperatorType.AND.value: 6,
    OperatorType.OR.value: 5,
    OperatorType.LEFT_PARENTHESIS.value: 4,
    OperatorType.RIGHT_PARENTHESIS.value: 3,
    OperatorType.IMPLIES.value: 2,
    OperatorType.BIDIRECTIONAL.value: 1
}


class Operator(NodeMixin):
    def __init__(self, op, value=None, parent=None):
        self.op = str(op)
        self.value = value
        self.parent = parent

    def __repr__(self):
        return 'Operator : {}'.format(self.op)


# Represents a single value argument
class Argument(NodeMixin):
    def __init__(self, arg, value=None, parent=None):
        self.arg = arg
        self.value = value
        self.parent = parent

    def __repr__(self):
        return 'Argument : {}'.format(self.arg)

