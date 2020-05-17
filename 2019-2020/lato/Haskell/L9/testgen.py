#! /usr/bin/python3
import random

def randomAssocs(n, start, end):
    assocs = []
    for i in range(n):
        key = random.randint(start, end)
        value = random.randint(start, end)
        assocs.append((key, value))
    return assocs

if __name__ == "__main__":
    print(randomAssocs(1000000, -10000, 10000))