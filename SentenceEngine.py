from utils import connectives


class SentenceEngine:
    def __init__(self, sentence):
        self.sentence = self._prepare(sentence)
        self.tokens = self.sentence.split(' ')

    def extract_proposition_symbols(self):
        symbols = {s for s in self.tokens}
        symbols = {s for s in symbols if s not in connectives}
        return symbols

    def _prepare(self, sentence):
        # remove extra whitespaces
        sentence = ' '.join(sentence.split(' '))
        return sentence

    def pair_exists(self, pos_symbol):
        neg_symbol = 'not ' + pos_symbol
        neg_symbol_exists = neg_symbol in self.sentence

        pos_symbol_exists = self.sentence.startswith(pos_symbol)

        if not pos_symbol_exists:
            pos_symbol_exists = any(
                symbol == pos_symbol and previous != 'not'
                for previous, symbol in zip(self.tokens, self.tokens[1:])
            )

        return neg_symbol_exists and pos_symbol_exists

    def remove_all_occurrences_of_pair(self, pos_symbol):
        neg_symbol = 'not ' + pos_symbol

        result = self.sentence.replace(neg_symbol, '')
        result = result.replace(pos_symbol, '')

        return result

