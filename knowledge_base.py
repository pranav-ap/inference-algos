from utils import ConnectiveType


class KnowledgeBase():
    def __init__(self, truth_values=None):
        self.sentences = []
        self.truth_values = truth_values or {}

    def size(self):
        return len(self.sentences)

    def tell(self, sentence):
        self.sentences.append(sentence)

    def retract(self, sentence):
        self.sentences.remove(sentence)

    def set_truth_value(self, symbol, value):
        self.truth_values[symbol] = value


def main():
    pass


if __name__ == '__main__':
    main()