from sentence_engine import SentenceEngine
from knowledge_base import KnowledgeBase
from utils import operators


def get_premise_and_conclusion(sentence):
    if '=>' in sentence:
        premise, conclusion = sentence.split('=>')
    else:
        premise, conclusion = '', sentence

    return premise.strip(), conclusion.strip()

def get_axioms(kb):
    return {sentence for sentence in kb.sentences if len(sentence.split()) == 1}

def pl_fc_entails(kb, query):
    agenda = get_axioms(kb)
    print('agenda : {}'.format(agenda))

    engine = SentenceEngine('{} and {}'.format(kb.as_sentence(), query))
    is_inferred = { symbol: False for symbol in engine.extract_proposition_symbols() }

    no_of_uninferred_premise_symbols = dict()

    for sentence in kb.sentences:
        premise, conclusion = get_premise_and_conclusion(sentence)
        no_of_uninferred_premise_symbols[sentence] = sum([1 for s in premise.split() if s not in operators])

    print('unresolved premise symbols for : {}'.format(no_of_uninferred_premise_symbols))

    while agenda:
        symbol = agenda.pop()
        if symbol == query:
            return True

        if is_inferred[symbol]:
            continue

        for sentence in kb.sentences:
            premise, conclusion = get_premise_and_conclusion(sentence)
            if symbol in premise:
                no_of_uninferred_premise_symbols[sentence] -= 1
            if no_of_uninferred_premise_symbols[sentence] == 0:
                agenda.add(conclusion)

        is_inferred[symbol] = True
    
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
