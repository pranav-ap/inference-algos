from utils import ConnectiveType
from parser import Parser, extract_preposition_symbols


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
    pass



if __name__ == '__main__':
    main()