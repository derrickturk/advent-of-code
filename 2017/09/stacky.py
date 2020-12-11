import sys

from enum import Enum, auto

from typing import NamedTuple

class State(Enum):
    Initial = auto()
    Group = auto()
    Garbage = auto()
    Cancel = auto()

class Metrics(NamedTuple):
    group_score: int
    garbage_count: int

# parse a stream and return its score and garbage count
def metrics(stream: str) -> Metrics:
    state_stack = [State.Initial]
    total_score = 0
    group_score = 0
    garbage_count = 0
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
                garbage_count += 1 # ignore
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
    return Metrics(total_score, garbage_count)

def main() -> int:
    stream = sys.stdin.read().strip()
    group_score, garbage = metrics(stream)
    print(group_score)
    print(garbage)
    return 0

if __name__ == '__main__':
    sys.exit(main())
