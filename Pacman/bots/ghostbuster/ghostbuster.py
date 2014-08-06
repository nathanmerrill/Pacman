#!/usr/bin/env python

import os
import re
import sys
import math
import random

sys.stdout = os.fdopen(sys.stdout.fileno(), 'w', 0) # automatically flush stdout

# read in the maze description
maze_desc = sys.stdin.readline().rstrip()
mazeSize = int(math.sqrt(len(maze_desc)))

North,East,South,West = range(4)
DIRECTIONS = ['N','E','S','W']
Euclidian,Manhattan,Chebyshev = range(3)
P,G,o,O,F,X = 5,200,-10,-100,-75,0

sign = lambda x: (1, -1)[x<0]
wrap = lambda v : v % mazeSize

class Node(object):

    def __init__(self, x, y, value):
        self.x, self.y = x,y
        self.wallValue = int(value, 16); #Base 16
        self.nodes = {}
        self.item = 'o' # Start as normal pellet

    def connect(self, otherNode, dir):
        if dir not in self.nodes:
            self.nodes[dir] = otherNode
            otherNode.nodes[(dir+2)%4] = self

    def distance(self, otherNode, meth = Euclidian):
        xd = otherNode.x - self.x
        yd = otherNode.y - self.y
        absxd = min(abs(xd), mazeSize - abs(xd))
        absyd = min(abs(yd), mazeSize - abs(yd))
        if meth == Euclidian:
            return math.sqrt(absxd * absxd + absxd * absxd)
        if meth == Manhattan:
            return absxd + absyd
        if meth == Chebyshev:
            return max(absxd, absyd)

    def direction(self, otherNode):
        for key, value in self.nodes.iteritems():
            if value == otherNode:
                return DIRECTIONS[key]
        return 'X'

    def getScore(self):
        score = eval(self.item)
        for node in self.nodes.values():
            score += eval(node.item)
        return score


    def __hash__(self):
        hValue = 17
        hValue = hValue*23 + hash(self.x)
        hValue = hValue*23 + hash(self.y)
        return hValue

    def __eq__(self, other):
        return (self.x,self.y) == (other.x, other.y)

    def __ne__(self, other):
        return (self.x,self.y) != (other.x, other.y)

    def __str__(self):
        return str(self.x)+","+str(self.y)

    def __repr__(self):
        return str(self.x)+","+str(self.y)


# Make all the nodes first
nodes = {}
i = 0
for y in range(mazeSize):
    for x in range(mazeSize):
        node = Node(x,y,maze_desc[i])
        nodes[x,y] = node
        i+=1

# Connect all the nodes together to form the maze
for node in nodes.values():
    walls = node.wallValue
    x,y = node.x,node.y
    if not walls&1:
        node.connect(nodes[x,wrap(y-1)], North)
    if not walls&2:
        node.connect(nodes[wrap(x+1),y], East)
    if not walls&4:
        node.connect(nodes[x,wrap(y+1)], South)
    if not walls&8:
        node.connect(nodes[wrap(x-1),y], West)

toVisit = set(nodes.values())

def aStar(startNode, endNode):
    openSet = set([startNode])
    closedSet = set()
    gScores = {startNode: 0}
    cameFrom = {}
    curNode = startNode
    while openSet:
        minF = 100000000
        for node in openSet:
            g = gScores[node]
            h = node.distance(endNode)
            f = g+h
            if f < minF:
                minF = f
                curNode = node

        if curNode == endNode:
            path = []
            while curNode != startNode:
                path.insert(0, curNode)
                curNode = cameFrom[curNode]
            return path

        openSet.remove(curNode)
        closedSet.add(curNode)
        for node in curNode.nodes.values():
            if node in closedSet:
                continue
            g = gScores[curNode] + node.getScore()
            isBetter = False
            if node not in openSet:
                openSet.add(node)
                isBetter = True
            elif g < gScores[node]:
                isBetter = True
            if isBetter:
                gScores[node] = g
                cameFrom[node]=curNode

# regex to parse a line of input
input_re = re.compile('(?:([-\d]+),([-\d]+)([PGoOFX]?) ?)+')

currentNode = None
destinationNode = None

while True:
    info = sys.stdin.readline().rstrip()
    if (not info) or (info == "Q"):
        break

    # break a line of input up into a list of tuples (X,Y,contents)
    info = [input_re.match(item).groups() for item in info.split()]

    # update what we know about all the cells we can see
    for cell in info:
        nodes[int(cell[0]),int(cell[1])].item = cell[2]

    # current location
    x = int(info[0][0])
    y = int(info[0][1])

    currentNode = nodes[x,y]

    while not destinationNode or destinationNode == currentNode:
        destinationNode = random.sample(toVisit, 1)[0]

    toVisit.discard(destinationNode)

    bestPath = aStar(currentNode, destinationNode)

    firstNode = bestPath[0]

    direction = currentNode.direction(firstNode)
    print direction
