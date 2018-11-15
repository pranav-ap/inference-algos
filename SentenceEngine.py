from utils import Operator, Argument, logical_precedence, operators, OperatorType
from anytree import RenderTree, findall
from copy import deepcopy


class SentenceEngine:
    def __init__(self, sentence):
        self.sentence = self._prepare(sentence)
        self.tokens = self._tokenize()
        self.expression_tree = self._get_expression_tree()

    def _prepare(self, sentence):
        # remove extra whitespaces
        sentence = ' '.join(sentence.split(' '))
        return sentence
    
    def _tokenize(self):
        previous = ''
        tokens = []
    
        for current in self.sentence.split(' ') + ['$']:
            if previous == '':
                previous = current
                continue
            
            if previous != OperatorType.NOT.value:
                tokens.append(previous)
                previous = current
            elif current not in operators:
                tokens.append(previous + ' ' + current)
                previous = ''
            else:
                tokens.append(previous)
                previous = current

        return tokens

    def _negate(self, symbol):
        return '{} {}'.format(OperatorType.NOT.value, symbol) if not symbol.startswith(OperatorType.NOT.value) else symbol
    
    def extract_proposition_symbols(self):
        symbols = {s for s in self.tokens}
        symbols = {s for s in symbols if s not in operators}
        return symbols

    def pair_exists(self, pos_symbol):
        neg_symbol = self._negate(pos_symbol)
        return pos_symbol in self.tokens and neg_symbol in self.tokens

    def is_disjunction(self):
        return not 'and' in self.tokens

    def is_conjunction(self):
        return not 'or' in self.tokens

    def _infix_to_postfix(self):
        stack = []
        output = []

        for token in self.sentence.split(' '):
            if token not in operators:
                output.append(token)
            elif token == OperatorType.LEFT_PARENTHESIS.value:
                stack.append(token)
            elif token == OperatorType.RIGHT_PARENTHESIS.value:
                # pop until a '(' is popped
                while stack and stack[-1] != OperatorType.LEFT_PARENTHESIS.value:
                    output.append(stack.pop())
                stack.pop()
            else:
                while stack and stack[-1] != OperatorType.LEFT_PARENTHESIS.value and logical_precedence[token] <= logical_precedence[stack[-1]]:
                    output.append(stack.pop())
                stack.append(token)

        # leftover
        while stack:
            output.append(stack.pop())

        return output

    def _get_expression_tree(self):
        stack = []
        postfix = self._infix_to_postfix()
        postfix = list(map(lambda arg: Operator(arg) if arg in operators else Argument(arg), postfix))

        root = postfix[-1]

        for node in postfix:
            if isinstance(node, Argument):
                stack.append(node)
            else:
                if node.op == OperatorType.NOT.value:
                    arg = stack.pop()
                    arg.parent = node
                else:
                    arg1 = stack.pop()
                    arg1.parent = node
                    arg2 = stack.pop()
                    arg2.parent = node

                stack.append(node)
        
        return root


class DisjunctionEngine(SentenceEngine):
    def __init__(self, clause):
        SentenceEngine.__init__(self, clause)
        if not SentenceEngine.is_disjunction(self):
            raise ValueError("Sentence is not a disjunct")
    
    def remove_pair_from_disjunction(self, pos_symbol):
        root = deepcopy(self.expression_tree)

        deathrow = findall(
            root,
            filter_=lambda node: isinstance(node, Argument) and node.arg == pos_symbol)
        
        for inmate in deathrow:
            if isinstance(inmate.parent, Operator) and inmate.parent.op == 'not':
                inmate = inmate.parent

            sibling = inmate.siblings[0] if inmate.siblings else None
            inmate.parent = None

            if sibling is not None:
                if sibling.parent.is_root:
                   sibling.parent = None
                   root = sibling
                elif isinstance(sibling, Operator):
                    for child in sibling.children:
                        child.parent = sibling.parent
                    sibling.parent = None
                elif isinstance(sibling, Argument):
                    parent = sibling.parent
                    sibling.parent = parent.parent
                    parent.parent = None

        return root



def main():
    # engine = SentenceEngine('( b11 <=> ( p12 or not p21 ) )')
    engine = DisjunctionEngine('( b or c or a or not b or c or b )')

    print(RenderTree(engine.expression_tree))
    print(RenderTree(engine.remove_pair_from_disjunction('b')))


if __name__ == '__main__':
    main()
