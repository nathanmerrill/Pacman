import Pacman
import Data.Complex
import Data.List
import Data.Function
import qualified Data.Map as Map
import Data.Maybe
import System.IO

data DebugInfo = DebugInfo {
  debugGrid :: Grid
, debugForce :: Force
, debugAction :: Action
} deriving (Show)

data Physicist = Physicist [(Int, Object)] (Maybe DebugInfo)

type Force = Complex Double


calcForce :: Int -> Position -> PlayerType -> Object -> Force
calcForce n p1 t1 object = if d2 == 0 then 0 else base / (fromIntegral d2 ** 1.5 :+ 0)
  where
    (x1, y1) = p1
    (x2, y2) = p2
    wrap d = minimumBy (compare `on` abs) [d, n - d]
    dx = wrap $ x2 - x1
    dy = wrap $ y2 - y1
    Object t2 p2 = object
    d2 = dx * dx + dy * dy
    base = (fromIntegral dx :+ fromIntegral dy) * case t1 of
      PacmanPlayer -> case t2 of
        Pellet -> 10.0
        PowerPellet -> 200.0
        Ghost -> -500.0
        Pacman -> -20.0
        Fruit -> 100.0
        Empty -> -2.0
      GhostPlayer -> case t2 of
        Pellet -> 10.0
        PowerPellet -> 200.0
        Ghost -> -50.0
        Pacman -> 500.0
        Fruit -> 100.0
        Empty -> -2.0

instance PlayerAI Physicist where
  findAction player info = (action, player') where
    Player {
      playerType = type_
    , playerField = field
    , playerMemory = Physicist objectsWithAge _
    } = player

    n = fieldSize field
    NormalRound pos _ objects = info
    objectsWithAge' = combineObjects objectsWithAge objects
    objects' = map snd objectsWithAge'
    directionChoices = filter (not . gridHasWall grid) directions4
    totalForce = sum $ map (calcForce n pos type_) objects'
    grid = fromMaybe (error $ "invalid position " ++ show pos) $ (fieldGetGrid field) pos
    action = if magnitude totalForce < 1e-10
      then if null directionChoices
        then Stay
        else Move $ head directionChoices
      else Move $ maximumBy (compare `on` (projectForce totalForce)) directionChoices
    debugInfo = Just $ DebugInfo grid totalForce action
    player' = player {
      playerMemory = Physicist objectsWithAge' debugInfo
    }

  -- roundDebug player _ = do
  --   let Physicist objects debugInfo = playerMemory player
  --       type_ = playerType player
  --   hPrint stderr (objects, debugInfo)

combineObjects :: [(Int, Object)] -> [Object] -> [(Int, Object)]
combineObjects xs ys = Map.elems $ foldr foldFunc initMap ys where
  foldFunc object@(Object type_ pos) = Map.insert pos (0, object)
  addAge (age, object) = (age + 1, object)
  toItem (age, object@(Object _ pos)) = (pos, (age, object))
  initMap = Map.fromList . map toItem . filter filterFunc . map addAge $ xs
  filterFunc (age, object@(Object type_ _))
    | type_ == Empty = True
    | age < maxAge = True
    | otherwise = False

maxAge = 5

projectForce :: Force -> Direction -> Double
projectForce (fx :+ fy) (Direction dx dy) = fx * fromIntegral dx + fy * fromIntegral dy

main :: IO ()
main = runAI $ Physicist [] Nothing
Pacman.hs

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