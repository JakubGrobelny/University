#! /usr/bin/python3
from typing import *
import z3
import json


class Ingredient:
    def __init__(self, properties: Dict[str, Union[float, str]]):
        self.name : str = properties['nazwa']
        del properties['nazwa']
        self.macros : Dict[str, float] = properties

class Target:
    def __init__(self, macro: str, targets: Dict[str, float]):
        self.macro : str = macro
        self.min : float = targets['min']
        self.max : float = targets['max']

class Instance:
    def __init__(self, filename: str):
        with open(filename) as file:
            instance = json.loads(file.read().replace('\n', ''))
        self.macros : List[str] = instance['parametry']
        self.ingredients : List[Ingredient] = []
        for ingredient in instance['składniki']:
            self.ingredients.append(Ingredient(ingredient))
        self.conflicts : Set[Tuple[str, str]] = set()
        for conflict in instance['konflikty']:
            first, second = conflict['nazwa1'], conflict['nazwa2']
            self.conflicts.add((first, second))
            self.conflicts.add((second, first))
        self.targets : List[Target] = []
        raw_targets = instance['cel']
        for macro in raw_targets.keys():
            self.targets.append(Target(macro, raw_targets[macro]))

def solve(instance: Instance) -> Optional[dict]:
    # TODO
    pass

def print_diet(diet: Dict[str, List[str]]):
    for meal in diet.keys():
        print(meal + ', '.join(diet[meal]))

if __name__ == "__main__":
    filename : str = input()
    instance = Instance(filename)
    result = solve(instance)
    if result is None:
        print("Nie można wygenerować diety.")
    else:
        print_diet(result)