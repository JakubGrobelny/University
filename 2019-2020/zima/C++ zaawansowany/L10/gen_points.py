#! /usr/bin/python3
import sys
import random

def generate_points(file, min_x, max_x, min_y, max_y, n):
    for i in range(0, n):
        x = random.uniform(min_x, max_x)
        y = random.uniform(min_y, max_y)
        r = random.randint(0, 255)
        g = random.randint(0, 255)
        b = random.randint(0, 255)
        name = 'point_{}'.format(i)
        file.write('{} {} {} {} {} {}\n'.format(x, y, r, g, b, name))

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: [filename]")
    else:
        filename = sys.argv[1]
        with open(filename, 'w') as file:
            generate_points(file, -100.0, 100.0, -100.0, 100.0, 64)