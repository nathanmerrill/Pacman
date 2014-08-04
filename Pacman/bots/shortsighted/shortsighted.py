#!/usr/bin/env python

import os
import re
import sys
import math
import random

sys.stdout = os.fdopen(sys.stdout.fileno(), 'w', 0) # automatically flush stdout

# read in the maze description
maze_desc = sys.stdin.readline().rstrip()
maze_size = int(math.sqrt(len(maze_desc)))

# turn the maze description into an array of arrays
# [wall bitmask, item last seen in square]

def chunks(l, n):
    for i in xrange(0, len(l), n):
        yield l[i:i+n]
maze = []
for row in chunks(maze_desc, maze_size):
    maze.append([[int(c,16),'X'] for c in row])

# regex to parse a line of input
input_re = re.compile('(?:([-\d]+),([-\d]+)([PGoOFX]?) ?)+')

turn = 0
invincibility_over = 0
last_move = None

while True:
    info = sys.stdin.readline().rstrip()
    if (not info) or (info == "Q"):
        break

    # break a line of input up into a list of tuples (X,Y,contents)
    info = info.split()
    info = [input_re.match(item).groups() for item in info]

    # update what we know about all the cells we can see
    for cell in info:
        maze[int(cell[1])][int(cell[0])][1] = cell[2]

    # current location
    x = int(info[0][0])
    y = int(info[0][1])

    # which directions can we move from our current location?
    valid_directions = []
    # we might consider sitting still
    # valid_directions.append('X')
    walls = maze[y][x][0]
    if not walls&1:
        valid_directions.append('N')
    if not walls&2:
        valid_directions.append('E')
    if not walls&4:
        valid_directions.append('S')
    if not walls&8:
        valid_directions.append('W')

    # which direction has the highest value item?
    best_value = 0
    best_direction = 'X'
    for c in [(x,y-1,'N'),(x+1,y,'E'),(x,y+1,'S'),(x-1,y,'W')]:
        if c[2] in valid_directions:
            # am I a ghost?
            if maze[y][x][1] == 'G':
                if maze[c[1]%maze_size][c[0]%maze_size][1] == "P":
                    best_value = 999
                    best_direction = c[2]
            else:
                if maze[c[1]%maze_size][c[0]%maze_size][1] == 'F':
                    if best_value < 100:
                        best_value = 100
                        best_direction = c[2]
                elif maze[c[1]%maze_size][c[0]%maze_size][1] == 'O':
                    if best_value < 50:
                        best_value = 50
                        best_direction = c[2]
                elif maze[c[1]%maze_size][c[0]%maze_size][1] == 'o':
                    if best_value < 10:
                        best_value = 10
                        best_direction = c[2]
                elif maze[c[1]%maze_size][c[0]%maze_size][1] == 'G':
                    if turn < invincibility_over:
                        # eat a ghost!
                        if best_value < 200:
                            best_value = 200
                            best_direction = c[2]
                    else:
                        # avoid the ghost
                        valid_directions.remove(c[2])

    # don't turn around, wasteful and dangerous
    if last_move:
        reverse = ['N','E','S','W'][['S','W','N','E'].index(last_move)]
        if reverse in valid_directions:
            valid_directions.remove(reverse)

    if best_value == 50:
        invincibility_over = turn + 10      
    if best_direction != 'X':
        # move towards something worth points
        # sys.stderr.write("good\n")
        last_move = best_direction
    elif len(valid_directions)>0:
        # move at random, not into a wall
        # sys.stderr.write("valid\n")
        last_move = random.choice(valid_directions)
    else:
        # bad luck!
        # sys.stderr.write("bad\n")
        last_move = random.choice(['N','E','S','W'])
    print last_move

    turn += 1
