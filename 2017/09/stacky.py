import sys

from enum import Enum, auto

# from typing import List

class State(Enum):
    Initial = auto()
    Group = auto()
    Garbage = auto()
    Cancel = auto()

# parse a stream and return its score
def score(stream: str) -> int:
    state_stack = [State.Initial]
    total_score = 0
    group_score = 0
    for c in stream:
        state = state_stack[-1] # "peek"
        if state == State.Cancel:
            state_stack.pop() # ignore one char
        elif state == State.Garbage:
            if c == '>':
                state_stack.pop()
            elif c == '!':
                state_stack.append(State.Cancel)
            else:
                pass # ignore
        elif state == State.Initial:
            if c == '{':
                state_stack.append(State.Group)
                group_score += 1
            else:
                raise ValueError('Expected "{"') 
        else: # in a group
            if c == '{':
                state_stack.append(State.Group)
                group_score += 1
            elif c == '}':
                if state == State.Initial:
                    raise ValueError('Expected "{"') 
                total_score += group_score
                group_score -= 1
                state_stack.pop()
            elif c == '<':
                state_stack.append(State.Garbage)
            elif c == ',':
                pass # ignore commas (technically, this will pass some
                     #   invalid strings, but we don't care...)
            else:
                raise ValueError('Expected "}" or "<"')
    return total_score

def main() -> int:
    stream = sys.stdin.read().strip()
    print(score(stream))
    return 0

if __name__ == '__main__':
    sys.exit(main())
