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
    axioms = get_axioms(kb)

    no_of_uninferred_premises = dict()
    for sentence in kb.sentences:
        premise, conclusion = get_premise_and_conclusion(sentence)
        no_of_uninferred_premises[sentence] = sum([1 for s in premise.split() if s not in operators])

    while axioms:
        axiom = axioms.pop()

        if axiom == query:
            return True
        
        index = 0
        while index < len(kb.sentences):
            sentence = kb.sentences[index]
            premise, conclusion = get_premise_and_conclusion(sentence)
            if axiom in premise:
                no_of_uninferred_premises[sentence] -= 1
                if no_of_uninferred_premises[sentence] == 0:
                    axioms.add(conclusion)
                    kb.tell(conclusion)
            index += 1
    
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

    print('size : {}'.format(kb.size()))
    print('Entails ? {}'.format(pl_fc_entails(kb, query)))
    print('size : {}'.format(kb.size()))
    print('kb : {}'.format(kb.as_sentence()))

if __name__ == '__main__':
    main()
