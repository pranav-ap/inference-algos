class KnowledgeBase():
    def __init__(self):
        self.sentences = []

    def size(self):
        return len(self.sentences)

    def tell(self, sentence):
        self.sentences.append(sentence)

    def retract(self, sentence):
        self.sentences.remove(sentence)


def main():
    pass


if __name__ == '__main__':
    main()