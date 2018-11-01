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


class Expr(object):
    def __init__(self, op, *args):
        self.op = str(op)
        self.args = args

    def valency_type(self):
        if len(self.args) == 1:
            return ValencyType.UNARY
        elif len(self.args) == 2:
            return ValencyType.BINARY
        else:
            return ValencyType.NULL



class KnowledgeBase():
    def __init__(self):
        self.sentences = []
        # self.parser = PLParser()

    def size(self):
        return len(self.sentences)

    def tell(self, sentence):
        self.sentences.append(sentence)

    def retract(self, sentence):
        self.sentences.remove(sentence)

    def as_sentence(self):
        single_sentence = ') {} ('.format(ConnectiveType.AND.value).join(self.sentences)
        single_sentence = '(' + single_sentence + ')'
        return single_sentence


def main():
    kb = KnowledgeBase()
    kb.tell('a and b => c')
    kb.tell('e and b => c')
    print(kb.as_sentence())



if __name__ == '__main__':
    main()