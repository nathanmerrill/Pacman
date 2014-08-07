#!/usr/bin/env python

import os
import re
import sys
import math
import random

sys.stdout = os.fdopen(sys.stdout.fileno(), 'w', 0) # automatically flush stdout

P,G,o,O,F,X = 10,500,-10,-75,-100,2
PreviousSquarePenalty = 100

# read in the maze description
maze_desc = sys.stdin.readline().rstrip()
mazeSize = int(math.sqrt(len(maze_desc)))

North,East,South,West = range(4)
DIRECTIONS = ['N','E','S','W']
Euclidian,Manhattan,Chebyshev = range(3)

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

    def distance(self, otherNode, meth = Manhattan):
        xd = abs(otherNode.x - self.x)        
        yd = abs(otherNode.y - self.y)
        xd = min(xd, mazeSize - xd)
        yd = min(yd, mazeSize - yd)
        if meth == Euclidian:
            return math.sqrt(xd * xd + yd * yd)       
        if meth == Manhattan:
            return xd + yd
        if meth == Chebyshev:      
            return max(xd, yd)

    def direction(self, otherNode):
        for key, value in self.nodes.iteritems():
            if value == otherNode:
                return DIRECTIONS[key]            
        return 'ERROR'

    def getScore(self):
        score = eval(self.item)
        for node in self.nodes.values():
            score += eval(node.item)
        return score

    def nearbyGhost(self):
        if self.item == 'G':
            return True
        for node in self.nodes.values():
            if node.item == 'G':
                return True
        return False

    def __hash__(self):  
        return  (391 + hash(self.x))*23 + hash(self.y)

    def __eq__(self, other):
        return (self.x, self.y) == (other.x, other.y)

    def __ne__(self, other):
        return (self.x, self.y) != (other.x, other.y)

    def __str__(self):
        return str(self.x)+","+str(self.y)

    def __repr__(self):
        return str(self.x)+","+str(self.y)

# Make all the nodes first
nodes = {}
i = 0
for y in range(mazeSize):
    for x in range(mazeSize):       
        nodes[x,y] = Node(x,y,maze_desc[i])  
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
currentNode = None
destinationNode = None
previousNode = None
testInvincibilty = False
invincibility = 0
isGhost = False

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
            g = gScores[curNode]
            if isGhost:
                g += 1
                if node.item == 'P':
                    g -= 10 # prefer PacMen
            else:
                s = node.getScore();
                if invincibility > 1:
                    g -= abs(s) # everything is tasty when invincible
                else:
                    g += s
                if previousNode and node == previousNode:
                    g += PreviousSquarePenalty # penalize previous square
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

while True:
    info = sys.stdin.readline().rstrip()
    if (not info) or (info == "Q"):
        break

    # break a line of input up into a list of tuples (X,Y,contents)
    info = [input_re.match(item).groups() for item in info.split()]

    # update what we know about all the cells we can see
    for cell in info:
        nodes[int(cell[0]),int(cell[1])].item = cell[2]

    currentNode = nodes[int(info[0][0]),int(info[0][1])]    

    if not isGhost and currentNode.item == 'G':
        isGhost = True
        destinationNode = random.sample(nodes.values(), 1)[0]

    if isGhost:     
        while destinationNode == currentNode or currentNode.distance(destinationNode) > 8:
            destinationNode = random.sample(nodes.values(), 1)[0]
    else:
        if invincibility > 0:
            invincibility -=  1

        if testInvincibilty:
            testInvincibilty = False
            if currentNode.item == 'X':
                invincibility += 10

        while not destinationNode or destinationNode == currentNode:
            destinationNode = random.sample(toVisit, 1)[0]

        toVisit.discard(currentNode)


    bestPath = aStar(currentNode, destinationNode)

    nextNode = bestPath[0]

    direction = currentNode.direction(nextNode)

    if not isGhost:
        if nextNode.item == 'O':   
            testInvincibilty = True      

    previousNode = currentNode

    print direction