import json
import sys


class Automaton: 
    
    def __init__(self, file_name: str):
    
        def map_states(states):
            i = 0
            mapping = {}
            for state in states:
                mapping[state] = i
                i += 1
            return mapping

        def parse_transitions(ids, transitions):
            ts = [{} for i in range(len(ids))]
            for t in transitions:
                st_from = ids[t["from"]]
                st_to   = ids[t["to"]]
                ts[st_from].update({t["letter"] : st_to})
            return ts

        with open(file_name, 'r') as file:
            contentJSON = file.read()
            content = json.loads(contentJSON)
            states_ids = map_states(content["states"])

        self.initial = states_ids[content["initial"]]
        self.accepting = [states_ids[x] for x in content["accepting"]]
        self.transitions = parse_transitions(states_ids, content["transitions"])
        self.current = self.initial

    def reset(self):
        self.current = self.initial

    def update(self, letter):
        try:
            self.current = self.transitions[self.current][letter]
        except:
            self.current = None
    
    def is_recognized_message(self):
        if self.current in self.accepting:
            print("yes")
        else:
            print("no")


def main():
    file_name = input()

    try:
        automaton = Automaton(file_name)
    except:
        print("Invalid automaton description!")
        return

    c = sys.stdin.read(1)
    is_empty = bool(c)

    while c:
        if c == '\n':
            automaton.is_recognized_message()
            automaton.reset()
            is_empty = True
        else:
            automaton.update(c)
            is_empty = False
        c = sys.stdin.read(1)

    if not is_empty:
        automaton.is_recognized_message()

main()