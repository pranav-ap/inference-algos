from enum import Enum

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


def pl_true(kb, model):
    pass