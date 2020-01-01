#! /usr/bin/python3
from typing import *
from z3     import *
import json


def solve(instance: dict) -> Optional[dict]:
    return None

def print_diet(diet: dict):
    pass

if __name__ == "__main__":
    filename = input()
    with open(filename) as file:
        instance = json.loads(file.read().replace('\n', ''))
    result = solve(instance)
    if result is None:
        print("Nie można wygenerować diety.")
    else:
        print_diet(result)