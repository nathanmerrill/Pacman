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

# turn the maze description into an array of arrays of numbers indicating wall positions

def chunks(l, n):
    for i in xrange(0, len(l), n):
        yield l[i:i+n]
maze = []
for row in chunks(maze_desc, maze_size):
    maze.append([[int(c,16),'X'] for c in row])

# regex to parse a line of input
input_re = re.compile('(?:([-\d]+),([-\d]+)([PGoOFX]?) ?)+')

last_moved = 'X'

while True:
    info = sys.stdin.readline().rstrip()
    if (not info) or (info == "Q"):
        break

    # break a line of input up into a list of tuples (X,Y,contents)
    info = info.split()
    info = [input_re.match(item).groups() for item in info]

    # location
    x = int(info[0][0])
    y = int(info[0][1])

    # update what we know about all the cells we can see
    for cell in info:
        maze[int(cell[1])][int(cell[0])][1] = cell[2]

    # which directions can we move from our current location?
    valid_directions = []
    walls = maze[y][x][0]
    if not walls&1: 
        valid_directions.append('N')
    if not walls&2:
        valid_directions.append('E')
    if not walls&4:
        valid_directions.append('S')
    if not walls&8:
        valid_directions.append('W')

    bad_directions = []
    for c in [(x,y-1,'N'),(x+1,y,'E'),(x,y+1,'S'),(x-1,y,'W')]:
        if c[2] in valid_directions:
            # am I a ghost?
            if maze[y][x][1] == 'G':
                # it's a pacman, run. interaction is always a risk.
                if maze[c[1]%maze_size][c[0]%maze_size][1] == "P":
                    valid_directions.remove(c[2])
                # another ghost? let me move away.
                elif maze[c[1]%maze_size][c[0]%maze_size][1] == "G":
                    bad_directions.append(c[2])
            else:
                # it's a ghost, run. ghosts are evil.
                if maze[c[1]%maze_size][c[0]%maze_size][1] == "G":
                    valid_directions.remove(c[2])
                # its another pacman, move away!
                elif maze[c[1]%maze_size][c[0]%maze_size][1] == "P":
                    bad_directions.append(c[2])

    # if possible to avoid normal contact, do so
    good_directions = list(set(valid_directions) - set(bad_directions))
    if len(good_directions) > 0:
        valid_directions = good_directions

    # if not the only option, remove going backwards from valid directions
    if len(valid_directions) > 1:
        if last_moved == 'N' and 'S' in valid_directions:
            valid_directions.remove('S')
        elif last_moved == 'S' and 'N' in valid_directions:
            valid_directions.remove('N')
        elif last_moved == 'W' and 'E' in valid_directions:
            valid_directions.remove('E')
        elif 'W' in valid_directions:
            valid_directions.remove('W')

    # if possible, continue in the same direction
    if last_moved in valid_directions:
        print last_moved
    # prefer turning left/right randomly instead of turning 180
    #   backwards has been removed from valid_directions if not
    #   the only option
    elif len(valid_directions) > 0:
        last_moved=random.choice(valid_directions)
        print last_moved
    # can't move, so stay put. desire to continue in original 
    #   direction remains.
    else:
        print 'X'