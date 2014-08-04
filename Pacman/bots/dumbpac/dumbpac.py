#!/usr/bin/env python

import os
import sys
import random

sys.stdout = os.fdopen(sys.stdout.fileno(), 'w', 0) # automatically flush stdout

maze_desc = sys.stdin.readline().rstrip()
while True:
    info = sys.stdin.readline().rstrip()
    if (not int) or (info == "Q"):
        break
    print random.choice(['N', 'E', 'S', 'W', 'X'])
