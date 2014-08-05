#!/usr/bin/env python

import math
import random
import sys
import subprocess
import os
import shlex
import select
import itertools
import time

ON_POSIX = 'posix' in sys.builtin_module_names
MAX_ROUNDS = 500
WINDOWS = False
try:
    select.poll()
    from interruptingcow import timeout
except:
    WINDOWS = True
    pass

class Player(object):
    def __init__(self, name, command):
        self.name = name
        self.direction = North
        self.square = None
        self.process = subprocess.Popen(shlex.split(command), stdout=subprocess.PIPE, stdin=subprocess.PIPE, bufsize=1, close_fds=ON_POSIX, cwd="bots/"+name+"/")
        if not WINDOWS:
            self.pollin = select.poll()
            self.pollin.register(self.process.stdin, select.POLLOUT)
        self.turns_invincible = 0
        self.score = 0
        self.is_ghost = False
        self.has_teleported = False
        self.round = 0
        self.time_limit = 10

    def start(self):
        maze_desc = ""
        # TODO: solve actual problem instead of hotfix with transposition here
        maze_transposed = zip(*self.square.maze.grid)
        # maze_transposed = self.square.maze.grid
        for line in maze_transposed:
            for square in line:
                maze_desc+=square.to_hex()
        self.send_message(maze_desc)

    def end_round(self):
        self.round += 1
        self.time_limit = 1

    def move(self):
        letters = ['P' if len(self.square.players) > 1 else 'X']
        letters = [",".join([str(x) for x in self.square.coordinates])]
        if self.is_ghost:
            letters[0] += "G"
        elif len(self.square.players) >1:
            letters[0] += "P"
        else:
            letters[0] += "X"
        if self.direction:
            d_index = directions.index(self.direction)
            follow_directions = [d_index-1, d_index, d_index-3]
        else:
            follow_directions = range(0, 4)
        for d in follow_directions:
            first_square = self.square
            cur_square = self.square
            next_square = cur_square.neighbors(wraps=True)[d]
            while cur_square.is_connected_to(next_square, wraps=True) and cur_square!=first_square:
                letters.append(",".join([str(x) for x in next_square.coordinates])+next_square.letter())
                cur_square = next_square
                next_square = next_square.neighbors(wraps=True)[d]

        for x in xrange(len(follow_directions)):
            cur_direction = follow_directions[x]
            prev_direction = follow_directions[x-1]
            if (not self.square.walls[cur_direction] and not self.square.neighbors(wraps=True)[cur_direction].walls[prev_direction])\
                    or (not self.square.walls[prev_direction] and not self.square.neighbors(wraps=True)[prev_direction].walls[cur_direction]):
                corner = self.square.neighbors(wraps=True)[cur_direction].neighbors(wraps=True)[prev_direction]
                letters.append(",".join([str(x) for x in corner.coordinates])+corner.letter())

        message = " ".join(letters)
        self.send_message(message)
        while True:
            move = str(self.get_response()).lower().strip()
            coord = move.split(",")
            if len(coord) != 2:
                break
            if coord[0].isdigit() and coord[1].isdigit():
                self.send_message(self.square.grid.get(coord).to_hex())
        if move=="n":
            direction = North
        elif move=="e":
            direction = East
        elif move=="w":
            direction = West
        elif move=="s":
            direction = South
        else:
            if move != 'x':
                self.send_message("Bad input:"+move)
            direction = None

        if direction:
            index = directions.index(direction)
            if self.square.walls[index]:
                if __debug__: print self.name + " walked " + move + " into a wall!"
                self.direction = None
            else:
                self.move_to(self.square.neighbors(wraps=True)[index])

    def check_square(self):
        if self.square.ghosts:
            invincibles = [player for player in self.square.players if player.turns_invincible]
            if invincibles:
                for ghost in self.square.ghosts:
                    ghost.teleport()
                for player in invincibles:
                    player.score += 200
            else:
                self.is_ghost = True
                all_ghosts.append(self)
                bots.remove(self)
                self.square.players.remove(self)
                self.square.ghosts.append(self)
            return
        if len(self.square.players) > 1:
            self.square.contents = Nothing
            return
        if self.square.contents is Nothing:
            return
        if self.square.contents is Pellet:
            self.square.maze.num_pellets -= 1;
            self.score += 10
        elif self.square.contents is PowerPellet:
            self.score += 50
            self.turns_invincible = 10
        elif self.square.contents is Fruit:
            self.score += 100
        self.square.contents = Nothing

    def get_response(self):
        if WINDOWS:
            while True:
                message = self.process.stdout.readline()
                if __debug__:
                    print "got message:"+message
                return message

        if __debug__: print "waiting for response from " + self.name
        try:
            with timeout(self.time_limit, exception=RuntimeError):
                response = self.process.stdout.readline()
                if __debug__: print "got response from " + self.name + " : " + response.rstrip()
                return response
        except RuntimeError:
            if __debug__: print "gave up on " + self.name
            self.remove()
            raise RuntimeError(self.name+" didn't produce a response within one second")

    def teleport(self):
        self.has_teleported = True
        self.move_to(self.square.maze.get((random.randrange(self.square.maze.side_length),random.randrange(self.square.maze.side_length))))


    def move_to(self, square):
        if self.is_ghost:
            if self.has_teleported:
                self.has_teleported = False
                return
            self.square.ghosts.remove(self)
            self.square = square
            self.square.ghosts.append(self)
        else:
            if self.turns_invincible:
                self.turns_invincible -= 1
            self.square.players.remove(self)
            self.square = square
            self.square.players.append(self)


    def send_message(self, message):
        if __debug__: print "send message to " + self.name + " : " + message
        if WINDOWS:
            self.process.stdin.write(message+"\n")
            self.process.stdin.flush()
            return
        try:
            with timeout(self.time_limit, exception=RuntimeError):
                while not self.pollin.poll(0):
                    time.sleep(0.1)
                self.process.stdin.write(message+"\n")
                if __debug__: print "sent message to " + self.name
        except RuntimeError:
            if __debug__: print "gave up on " + self.name
            self.remove()
            raise RuntimeError(self.name+" didn't accept a message within one second")

    def remove(self):
        self.square.players.remove(self)


class Ghost(object):
    def __init__(self, start_square):
        self.duration = random.randrange(5,12)
        self.count = 0
        self.chasing = False
        self.square = start_square
        self.closest_player = None
        self.last_square = None
        self.has_teleported = False
        self.round = 0

    def end_round(self):
        self.round += 1
        self.time_limit = 1

    def teleport(self):
        self.step(self.square.maze.get((random.randrange(self.square.maze.side_length),random.randrange(self.square.maze.side_length))))
        self.has_teleported = True

    def move(self):
        self.count += 1
        if self.count is self.duration:
            self.chasing = not self.chasing
            self.count = 0
            if self.chasing:
                players = []
                distance = 100
                for x in xrange(-5, 6):
                    for y in xrange(-5, 6):
                        next = self.square.maze.get((self.square.x+x, self.square.y+y), wraps=True)
                        if next.players:
                            if abs(x)+abs(y) < distance:
                                players = []
                                distance = abs(x) + abs(y)
                            if abs(x)+abs(y)==distance:
                                players.extend(next.players)
                if players:
                    self.closest_player = random.choice(players)
                    self.switch()
                    self.step_to(self.last_square.coordinates, can_reverse=True)
                    return
                else:
                    self.chasing = False
        if self.chasing:
            self.chase()
        else:
            self.scatter()

    def scatter(self):
        neighbors = self.square.neighbors(connected=True, wraps=True)
        try:
            neighbors.remove(self.last_square)
        except ValueError:
            pass
        try:
            self.step(random.choice(neighbors))
        except IndexError:
            import pdb
            pdb.set_trace()

    def chase(self):
        pass

    def switch(self):
        pass

    def step(self, to):
        if self.has_teleported:
            self.has_teleported = False
            return
        self.square.ghosts.remove(self)
        self.last_square = self.square
        to.ghosts.append(self)
        self.square = to


    def step_to(self, point, can_reverse=False):
        top_score = -50
        next_direction = None
        for index, direction in enumerate(directions):
            if self.last_square and direction + self.square == self.last_square.coordinates:
                continue
            if self.square.walls[index]:
                continue
            score = [(finish-start)*dir for start, finish, dir in zip(self.square.coordinates, point, direction.get_coordinates())]
            score = score[0] if score[0] else score[1]
            if score > 10:
                score -= self.square.maze.side_length
            elif score < -10:
                score += self.square.maze.side_length
            if score > top_score:
                top_score = score
                next_direction = direction
        if next_direction:
            self.step(self.square.maze.get((next_direction+self.square).get_coordinates(), wraps=True))


class Pinky(Ghost):
    def chase(self):
        if self.closest_player.direction:
            next_square = [direction*4 + coordinate for direction, coordinate in zip(self.closest_player.direction.get_coordinates(), self.closest_player.square.coordinates)]
        else:
            next_square = self.closest_player.square.coordinates
        self.step_to(next_square)


class Inky(Ghost):
    def __init__(self, start_square):
        Ghost.__init__(self, start_square)
        self.closest_ghost = None

    def chase(self):
        if self.closest_player.direction:
            player_square = [direction*2 + coordinate for direction, coordinate in zip(self.closest_player.direction.get_coordinates(), self.closest_player.square.coordinates)]
        else:
            player_square = self.closest_player.square.coordinates
        next_square = [a*2-b for a, b in zip(self.closest_ghost.square.coordinates,self.square.coordinates)]
        self.step_to(next_square)


    def switch(self):
        ghosts = []
        distance = 100
        for x in xrange(-5, 6):
            for y in xrange(-5, 6):
                next = self.square.maze.get((self.square.x+x, self.square.y+y), wraps=True)
                if next.ghosts:
                    if abs(x)+abs(y) < distance:
                        ghosts = []
                        distance = abs(x) + abs(y)
                    if abs(x)+abs(y)==distance:
                        ghosts.extend(next.ghosts)
        self.closest_ghost = random.choice(ghosts)


class Blinky(Ghost):
    def chase(self):
        next_square = self.closest_player.square
        self.step_to(next_square.coordinates)


class Clyde(Ghost):
    def __init__(self, start_square):
        Ghost.__init__(self, start_square)
        self.furthest_player = None

    def chase(self):
        self.step_to(self.furthest_player.square.coordinates)

    def switch(self):
        players = []
        distance = 0
        for x in xrange(-8, 9):
            for y in xrange(-8, 9):
                next = self.square.maze.get((self.square.x+x, self.square.y+y), wraps=True)
                if next.players:
                    if abs(x)+abs(y) > distance:
                        players = []
                        distance = abs(x) + abs(y)
                    if abs(x)+abs(y)==distance:
                        players.extend(next.players)
        self.furthest_player = random.choice(players)

ghosts = Inky, Blinky, Clyde, Pinky


class MazeGraphics:
    def __init__(self, maze):
        self.has_pygame = True
        self.maze = maze
        try:
            import pygame
        except ImportError:
            self.has_pygame = False
            print "ERROR: Pygame not installed"
            return
        self.square_size = 15
        pygame.display.init()
        self.bg_color = (0, 0, 0)
        self.fg_color = (0, 0, 255)
        self.dimensions = [maze.side_length*self.square_size]*2
        self.screen = pygame.display.set_mode(self.dimensions, 0, 32)
        pygame.display.set_caption("Maze")
        pygame.display.flip()

    def draw_maze(self):
        if self.has_pygame:
            for line in self.maze.grid:
                for square in line:
                    self.draw_square(square)
            self.update()
        else:
            top_str = ""
            bottom_str = ""
            for line in self.maze.grid:
                for square in line:
                    top_str += "*"
                    if square.walls[0]:
                        top_str += "*"
                    else:
                        top_str += " "
                    if square.walls[3]:
                        bottom_str += "|"
                    else:
                        bottom_str += " "
                    bottom_str += square.letter()
                print top_str
                print bottom_str
                top_str = ""
                bottom_str = ""
            print "\r\n"
                    

    def update(self):
        import pygame
        self.screen.blit(self.screen, (0, 0))
        pygame.display.update()
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                sys.exit(0)

    def draw_square(self, square):
        square_coordinates = [a*self.square_size for a in square.coordinates]
        import pygame
        rect = pygame.Rect(square_coordinates, (self.square_size, self.square_size))
        self.screen.fill(self.bg_color, rect)
        for index, wall in enumerate(square.walls):
            if not wall:
                continue
            line_coordinate = [[[0, 0], [1, 0]],
                               [[1, 0], [1, 1]],
                               [[1, 1], [0, 1]],
                               [[0, 1], [0, 0]]][index]
            offset_coordinate = []
            for point in line_coordinate:
                offset_coordinate.append([])
                for coordinate, offset in zip(point, square_coordinates):
                    offset_coordinate[-1].append(coordinate*(self.square_size-1) + offset)
            pygame.draw.line(self.screen, self.fg_color, offset_coordinate[0], offset_coordinate[1], 1)
        circle_color = (((0, 0, 0), (255, 0, 0), (100, 200, 200), (100, 200, 200))[square.contents] if not square.ghosts else (0, 255, 0)) if not square.players else (255, 255, 0)
        circle_size = self.square_size/6 if square.contents is Pellet and not square.ghosts and not square.players else self.square_size/3
        circle_offset = [coordinate*self.square_size + self.square_size/2 for coordinate in square.coordinates]
        pygame.draw.circle(self.screen, circle_color, circle_offset, circle_size)



class Direction(object):

    def __init__(self, x, y):
        self.x = x
        self.y = y

    def __add__(self, other):
        return Direction(self.x + other.x,self.y + other.y)

    def get_opposite(self):
        return directions[directions.index(self)-2]

    def get_coordinates(self):
        return self.x , self.y


North = Direction(0, -1)
East = Direction(1, 0)
South = Direction(0, 1)
West = Direction(-1, 0)
directions = [North, East, South, West]


class Square(object):
    def __init__(self, maze, coordinates):
        self.x, self.y = coordinates
        self.coordinates = coordinates
        self.maze = maze
        self.walls = [True]*4
        self.players = []
        self.ghosts = []
        self.contents = Nothing

    def __str__(self):
        return str(self.x)+","+str(self.y)

    def neighbors(self, connected=None, **kwargs):
        return [x for x in [self.maze.get((x+self).get_coordinates(), **kwargs) for x in directions] if connected==None or (self.is_connected_to(x, **kwargs)==connected)]

    def is_connected_to(self, other, **kwargs):
        try:
            return not self.walls[self.neighbors(**kwargs).index(other)]
        except IndexError:
            return False

    def connect_to(self, other, **kwargs):
        try:
            index = self.neighbors(**kwargs).index(other)
            self.walls[index] = False
            other.walls[index-2] = False
        except ValueError:
            pass

    def to_hex(self):
        num = 0
        for index, wall in enumerate(self.walls):
            num += 2**index if wall else 0
        return hex(num)[2:]

    def letter(self):
        if self.players:
            return 'P'
        elif self.ghosts:
            return 'G'
        else:
            return ['X','F','O','o'][self.contents]

Nothing, Fruit, PowerPellet, Pellet = range(4)


class Maze(object):
    def __init__(self, num_players):
        self.num_players = max(num_players, 1)
        self.side_length = int(math.ceil(math.sqrt(self.num_players)*10))
        self.num_ghosts = 2*self.num_players
        self.num_power_pellets = 4*self.num_players
        self.num_fruit = 2*self.num_players
        self.num_pellets = self.side_length**2 - self.num_ghosts - self.num_power_pellets - self.num_fruit - self.num_players
        self.grid = [[Square(self, (b, a)) for a in xrange(self.side_length)] for b in xrange(self.side_length)]

    def get(self, coordinates, wraps=False):
        try:
            return self.grid[coordinates[0]][coordinates[1]]
        except IndexError:
            if not wraps:
                return None
            coordinates = list(coordinates)
            while coordinates[0] < 0:
                coordinates[0] += self.side_length
            while coordinates[0] >= self.side_length:
                coordinates[0] -= self.side_length
            while coordinates[1] < 0:
                coordinates[1] += self.side_length
            while coordinates[1] >= self.side_length:
                coordinates[1] -= self.side_length
            return self.grid[coordinates[0]][coordinates[1]]

    def generate(self):
        start_square = self.get((random.randrange(self.side_length),
                                 random.randrange(self.side_length)))
        to_process = [(start_square,start_square.neighbors(wraps=False, connected=False))]
        while to_process:
            random.shuffle(to_process)
            next_square = to_process[-1][0]
            unconnected_neighbors = [x for x in to_process[-1][1] if x and all(x.walls)]
            if unconnected_neighbors:
                random.shuffle(unconnected_neighbors)
                connection = unconnected_neighbors.pop()
                next_square.connect_to(connection)
                if not unconnected_neighbors:
                    to_process.pop()
                connection_neighbors = connection.neighbors(connected=False, wraps=False)
                if connection_neighbors:
                    to_process.append((connection, connection_neighbors))
            else:
                to_process.pop()
        for line in self.grid:
            for square in line:
                if len(square.neighbors(connected=True, wraps=True)) == 1:
                    connections = square.neighbors(wraps=True, connected=False)
                    random.shuffle(connections)
                    for connection in connections:
                        if len(connection.neighbors(connected=True, wraps=True)) == 1:
                            square.connect_to(connection, wraps=True)
                            break
                    else:
                        if connections[0] in square.neighbors(connected=True, wraps=True):
                            square.connect_to(connections[1], wraps=True)
                        else:
                            square.connect_to(connections[0], wraps=True)
        to_place = (PowerPellet, self.num_power_pellets), (Fruit, self.num_fruit)
        for item, amount in to_place:
            while amount > 0:
                square = self.get((random.randrange(self.side_length),random.randrange(self.side_length)))
                if square.contents is Nothing:
                    square.contents = item
                    amount -= 1

        for line in self.grid:
            for square in line:
                if square.contents is Nothing:
                    square.contents = Pellet
        for _ in xrange(self.num_ghosts):
            while True:
                square = self.get((random.randrange(self.side_length), random.randrange(self.side_length)))
                if square.contents is Pellet:
                    square.contents = Nothing
                    square.ghosts.append(random.sample(ghosts, 1)[0](square))
                    all_ghosts.append(square.ghosts[-1])
                    break

        for bot in bots:
            while True:
                square = self.get((random.randrange(self.side_length),random.randrange(self.side_length)))
                if square.contents is not Pellet:
                    continue
                for neighbor in square.neighbors(connected=True, wraps=True):
                    if neighbor.contents is not Pellet:
                        break
                else:
                    square.contents = Nothing
                    square.players.append(bot)
                    bot.square = square
                    break


def run_programs():
    players = bots[:]
    for bot in bots:
        bot.start()
    round = 0
    while round < MAX_ROUNDS:
        if not bots:
            break
        for ghost in all_ghosts:
            ghost.move()
        for bot in bots[:]:
            bot.check_square()
        for bot in bots:
            bot.move()
        for bot in bots[:]:
            bot.check_square()
        for spite in itertools.chain(all_ghosts, bots):
            spite.end_round()

        graphics.draw_maze()

        round += 1
    for player in players:
        player.send_message("Q")
    if not WINDOWS:
        for player in players:
            try:
                with timeout(2, exception=RuntimeError):
                    player.process.wait()
            except RuntimeError:
                pass


def generate_maze():
    global bots
    maze = Maze(len(bots))
    maze.generate()
    return maze


def read_bot_list():
    players = []
    for dir in os.listdir(r"bots/"):
        try:
            file = open("bots/"+dir+"/command.txt", 'r')
        except IOError:
            continue
        command = file.read()
        file.close()
        for x in xrange(1):
            if command:
                players.append(Player(dir, command))
    return players

if __name__ == "__main__":
    random.seed()
    bot_scores = {}
    if __debug__:
        repeats = 1
    else:
        repeats = 50
    for x in xrange(repeats):
        bots = read_bot_list()
        all_ghosts = []
        maze = generate_maze()
        graphics = MazeGraphics(maze)
        graphics.draw_maze()
        run_programs()
                
        for bot in [bot for bot in all_ghosts if isinstance(bot, Player)]:
            if bot.name not in bot_scores:
                bot_scores[bot.name] = bot.score
            else:
                bot_scores[bot.name] += bot.score
    for bot_name in bot_scores:
        print bot_name+": "+str(bot_scores[bot_name])+" points"
