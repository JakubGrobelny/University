#! /usr/bin/python3
from typing import Dict, List, Union, Optional
import z3
import json


class Ingredient:
    def __init__(self, properties: Dict[str, Union[float, str]]):
        self.name : str = properties['nazwa']
        del properties['nazwa']
        self.macros : Dict[str, float] = properties

class Instance:
    def __init__(self, filename: str):
        with open(filename) as file:
            instance = json.loads(file.read().replace('\n', ''))
        self.macros : List[str] = instance['parametry']
        self.ingredients = []
        for ingredient in instance['składniki']:
            self.ingredients.append(Ingredient(ingredient))
        self.conflicts = set()
        for conflict in instance['konflikty']:
            first, second = conflict['nazwa1'], conflict['nazwa2']
            self.conflicts.add((first, second))
            self.conflicts.add((second, first))
        self.target = instance['cel']

def solve(instance: Instance) -> Optional[dict]:
    return None

def print_diet(diet: dict):
    pass

if __name__ == "__main__":
    filename = input()
    instance = Instance(filename)
    result = solve(instance)
    if result is None:
        print("Nie można wygenerować diety.")
    else:
        print_diet(result)