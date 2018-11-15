from SentenceEngine import SentenceEngine
from knowledge_base import KnowledgeBase
from utils import operators


def get_premise_and_conclusion(sentence):
    if '=>' in sentence:
        premise, conclusion = sentence.split('=>')
    else:
        premise, conclusion = sentence, sentence

    return premise.strip(), conclusion.strip()


def pl_fc_entails(kb, query):
    engine = SentenceEngine('{} and {}'.format(kb.as_sentence(), query))
    agenda = engine.extract_proposition_symbols()
    inferred = { symbol: False for symbol in agenda }
    count = dict()

    for sentence in kb.sentences:
        premise, conclusion = get_premise_and_conclusion(sentence)
        symbol_count = sum([1 for s in premise.split() if s not in operators])
        count[conclusion] = symbol_count

    while agenda:
        p = agenda.pop()
        if p == query:
            return True

        if inferred[p]:
            continue

        inferred[p] = True

        for sentence in kb.sentences:
            premise, conclusion = get_premise_and_conclusion(sentence)
            if p in premise:
                count[conclusion] -= 1
                if count[conclusion] == 0:
                    agenda.add(conclusion.strip())

    return False


def main():
    kb = KnowledgeBase()
    kb.tell('a')
    kb.tell('b')
    kb.tell('a and b => l')
    kb.tell('a and p => l')
    kb.tell('b and l => m')
    kb.tell('l and m => p')
    kb.tell('p => q')

    query = 'q'

    print('Entails ? {}'.format(pl_fc_entails(kb, query)))


if __name__ == '__main__':
    main()
