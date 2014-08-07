module Pacman (
    Field(..)
  , Grid(..)
  , Direction(..)
  , directions4, directions8
  , Position
  , newPosition
  , Player(..)
  , PlayerType(..)
  , ObjectType(..)
  , Object(..)
  , RoundInfo(..)
  , Action(..)
  , runAI
  , PlayerAI(..)
  ) where

import Data.Bits
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Map as Map
import qualified System.IO as SysIO

data Field = Field {
  fieldGetGrid :: Position -> Maybe Grid
, fieldSize :: Int
}

data Grid = Grid {
  gridHasWall :: Direction -> Bool
, gridPos :: Position
}

instance Show Grid where
  show g = "Grid " ++ show (gridPos g) ++ ' ':reverse [if gridHasWall g d then '1' else '0' | d <- directions4]

data Direction = Direction Int Int
  deriving (Show, Eq)

directions4, directions8 :: [Direction]
directions4 = map (uncurry Direction) [(-1, 0), (0, 1), (1, 0), (0, -1)]
directions8 = map (uncurry Direction) $ filter (/=(0, 0)) [(dx, dy) | dx <- [-1..1], dy <- [-1..1]]

type Position = (Int, Int)
newPosition :: (Int, Int)  -> Position
newPosition = id

data Player a = Player {
  playerType :: PlayerType
, playerField :: Field
, playerRounds :: Int
, playerMemory :: a
}
data PlayerType = PacmanPlayer | GhostPlayer
  deriving (Show, Eq)

class PlayerAI a where
  onGameStart :: a -> Field -> IO ()
  onGameStart _ _ = return ()

  onGameEnd :: a -> IO ()
  onGameEnd _ = return ()

  findAction :: Player a -> RoundInfo -> (Action, Player a)

  roundDebug :: Player a -> RoundInfo -> IO ()
  roundDebug _ _ = return ()


data ObjectType = Pacman | Ghost | Fruit | Pellet | PowerPellet | Empty
  deriving (Eq, Show)
data Object = Object ObjectType Position
  deriving (Show)

data RoundInfo = EndRound | NormalRound Position PlayerType [Object]

data Action = Stay | Move Direction
  deriving (Show)


parseField :: String -> Field
parseField s = if validateField field
  then field 
  else error $ "Invalid field: " ++ show ("n", n, "s", s, "fieldMap", fieldMap)
  where
    field = Field {
      fieldGetGrid = flip Map.lookup fieldMap
    , fieldSize = n
    }
    (n : _) = [x | x <- [0..], x * x == length s]
    fieldMap = Map.fromList [
        ((i, j), parseGrid c (newPosition (i, j))) 
        | (i, row) <- zip [0..n-1] rows,
          (j, c) <- zip [0..n-1] row
      ]
    rows = reverse . snd $ foldr rowFoldHelper (s, []) [1..n]
    rowFoldHelper _ (s, rows) =
      let (row, s') = splitAt n s
      in (s', row:rows)

validateField :: Field -> Bool
validateField field@(Field { fieldGetGrid=getGrid, fieldSize=n }) = 
  all (validateGrid field) $ map (fromJust.getGrid) [(i, j) | i <- [0..n-1], j <- [0..n-1]]

validateGrid :: Field -> Grid -> Bool
validateGrid
  field@(Field { fieldGetGrid=getGrid, fieldSize=n })
  grid@(Grid { gridPos=pos })
  = all (==True) [gridHasWall grid d == gridHasWall (getNeighbour d) (reverse d) | d <- directions4]
  where
    reverse (Direction dx dy) = Direction (-dx) (-dy)
    (x, y) = pos
    getNeighbour (Direction dx dy) = fromJust . getGrid . newPosition $ (mod (x + dx) n, mod (y + dy) n)

parseGrid :: Char -> Position -> Grid
parseGrid c pos = Grid gridHasWall pos
  where
    walls = zip directions4 bits
    bits = [((x `shiftR` i) .&. 1) == 1 | i <- [0..3]]
    Just x = elemIndex (toLower c) "0123456789abcdef"
    gridHasWall d = fromMaybe (error $ "No such direction " ++ show d) $
      lookup d walls

parseRoundInfo :: String -> RoundInfo
parseRoundInfo s = if s == "Q" then EndRound else NormalRound pos playerType objects'
  where
    allObjects = map parseObject $ words s
    Object type1 pos : objects = allObjects
    objects' = if type1 `elem` [Empty, Ghost] then objects else allObjects
    playerType = case type1 of
      Ghost -> GhostPlayer
      _ -> PacmanPlayer

parseObject :: String -> Object
parseObject s = Object type_ (newPosition (x, y)) where
  (y, x) = read $ "(" ++ init s ++ ")"
  type_ = case last s of
    'P' -> Pacman
    'G' -> Ghost
    'o' -> Pellet
    'O' -> PowerPellet
    'F' -> Fruit
    'X' -> Empty
    c -> error $ "Unknown object type: " ++ [c]

sendAction :: Action -> IO ()
sendAction a = putStrLn name >> SysIO.hFlush SysIO.stdout where
  name = (:[]) $ case a of
    Stay -> 'X'
    Move d -> fromMaybe (error $ "No such direction " ++ show d) $
      lookup d $ zip directions4 "NESW"

runPlayer :: PlayerAI a => Player a -> IO ()
runPlayer player = do
  roundInfo <- return . parseRoundInfo =<< getLine
  case roundInfo of
    EndRound -> return ()
    info@(NormalRound _ type_' _) -> do
      let
        updateType :: Player a -> Player a
        updateType player = player { playerType = type_' }
        player' = updateType player
        (action, player'') = findAction player' info
      roundDebug player'' info
      sendAction action
      let 
        updateRounds :: Player a -> Player a
        updateRounds player = player { playerRounds = playerRounds player + 1}
        player''' = updateRounds player''
      runPlayer player'''

runAI :: PlayerAI a => a -> IO ()
runAI mem = do
  field <- return . parseField =<< getLine
  let player = Player {
    playerType = PacmanPlayer
  , playerField = field
  , playerRounds = 0
  , playerMemory = mem
  }
  runPlayer player