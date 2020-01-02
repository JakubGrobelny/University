#! /usr/bin/python3
from typing import *
import z3
import json


MEAL_NAMES = [
    'śniadanie',
    'lunch',
    'obiad',
    'podwieczorek',
    'kolacja'
]

class Ingredient:
    def __init__(self, properties: Dict[str, Union[float, str]]):
        self.name : str = properties['nazwa']
        del properties['nazwa']
        self.macros : Dict[str, float] = properties

class Target:
    def __init__(self, macro: str, targets: Dict[str, float]):
        self.macro : str = macro
        self.min = float(targets['min'])
        self.max = float(targets['max'])

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
        self.targets : List[Target] = []
        raw_targets = instance['cel']
        for macro in raw_targets.keys():
            self.targets.append(Target(macro, raw_targets[macro]))

def optimize(instance: Instance):
    # TODO implement optimizations
    return instance

def make_variable_name(meal: int, ingredient: int) -> str:
    return 'var_' + str(ingredient) + '/' + str(meal)

def prepare_solver(instance: Instance) -> Tuple[z3.Solver, Dict[str, int]]:
    solver = z3.Solver()
    variables = []
    ingredient_indices : Dict[str, int] = {}
    num_of_meals = len(MEAL_NAMES)
    # create a list of variables for each ingredient
    for ingredient_index in range(len(instance.ingredients)):
        meal_vars = []
        name = instance.ingredients[ingredient_index].name
        ingredient_indices[name] = ingredient_index
        for meal_index in range(num_of_meals):
            var_name = make_variable_name(meal_index, ingredient_index)
            var = z3.Int(var_name)
            solver.add(var >= 0)
            meal_vars.append(z3.ToReal(var))
        variables.append(meal_vars)
    # add constraints for food conflicts
    for food1, food2 in instance.conflicts:
        food1_index = ingredient_indices[food1]
        food2_index = ingredient_indices[food2]
        food1_vars = variables[food1_index]
        food2_vars = variables[food2_index]
        for var1, var2 in zip(food1_vars, food2_vars):
            solver.add(z3.Or(var1 == 0, var2 == 0))
    # add constraints for total amount of substances
    macro_sums = {}
    for macro in instance.macros:
        macro_sums[macro] = 0.0
    for ingredient_vars, ingredient in zip(variables, instance.ingredients):
        ingredient_sum = z3.Sum(ingredient_vars)
        for macro in instance.macros:
            macro_sums[macro] += ingredient_sum * ingredient.macros[macro]
    for target in instance.targets:
        macro_sum = macro_sums[target.macro]
        solver.add(macro_sum >= target.min)
        solver.add(macro_sum <= target.max)
    return solver, ingredient_indices

def unpack_diet(result: z3.Model, food: Dict[str, int]) -> Dict[str, List[str]]:
    # TODO:
    return {}


def solve(instance: Instance) -> Optional[Dict[str, List[str]]]:
    optimize(instance)
    solver, food_indices = prepare_solver(instance)
    if solver.check() == z3.sat:
        return unpack_diet(solver.model(), food_indices)
    return None

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