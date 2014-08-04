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
map = []
for row in chunks(maze_desc, maze_size):
    map.append([int(c,16) for c in row])
# regex to parse a line of input
input_re = re.compile('(?:([-\d]+),([-\d]+)([PGoOFX]?) ?)+')

while True:
    info = sys.stdin.readline().rstrip()
    if (not info) or (info == "Q"):
        break

    # break a line of input up into a list of tuples (X,Y,contents)
    info = info.split()
    info = [input_re.match(item).groups() for item in info]

    # this pac only cares about its current location
    info = info[0]

    # which directions can we move from our current location?
    valid_directions = []
    # we might consider sitting still
    # valid_directions.append('X')
    walls = map[int(info[1])][int(info[0])]
    if not walls&1:
        valid_directions.append('N')
    if not walls&2:
        valid_directions.append('E')
    if not walls&4:
        valid_directions.append('S')
    if not walls&8:
        valid_directions.append('W')

    # move at random, not into a wall
    print random.choice(valid_directions)
