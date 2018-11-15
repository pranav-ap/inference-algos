from utils import Operator, Argument, logical_precedence, operators, OperatorType
from anytree import RenderTree, findall
from copy import deepcopy


class SentenceEngine:
    def __init__(self, sentence):
        self.sentence = self._prepare(sentence)
        self.tokens = self._tokenize()
        self.expression_tree = self.get_expression_tree()

    def _prepare(self, sentence):
        # remove extra whitespaces
        sentence = ' '.join(sentence.split(' '))
        return sentence
    
    def _tokenize(self):
        dumb_tokens = self.sentence.split(' ') + ['$']
        previous_token = ''
        tokens = []
    
        for current_token in dumb_tokens:
            if previous_token == '':
                previous_token = current_token
                continue
            
            if previous_token != OperatorType.NOT.value:
                tokens.append(previous_token)
                previous_token = current_token
            elif current_token not in operators:
                tokens.append(previous_token + ' ' + current_token)
                previous_token = ''
            else:
                tokens.append(previous_token)
                previous_token = current_token

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

    def remove_pair_from_clause(self, pos_symbol):
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

    def get_expression_tree(self):
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

    def is_valid(self):
        pass


def main():
    engine = SentenceEngine('( b or c or b or not b or c or b )')

    # engine = sentenceEngine('( b11 <=> ( p12 or not p21 ) )')
    expression_tree = engine.get_expression_tree()
    print(RenderTree(expression_tree))

    print(RenderTree(engine.remove_pair_from_clause('b')))


if __name__ == '__main__':
    main()
