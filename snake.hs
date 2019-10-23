-- followed steps of http://andrew.gibiansky.com/blog/haskell/haskell-gloss/ which created pong using gloss
module Main(main) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random
import System.IO.Unsafe
import Graphics.Gloss.Data.ViewPort
import Control.Monad.Random

-- PLAY
main :: IO ()
main = play glossGUI bgcol fps initSnake render controls renderAnimate

-- data for game
data Game = Game
  { headPos :: Coord,
    prevPos :: [Coord],
    bodyLength :: Float, -- we can also use this to display score as long as we subtract initial amount before displaying
    moveDirection :: Char,
--    headPos2 :: Coord,
--    prevPos2 :: [Coord],
--    bodyLength2 :: Int
--    moveDirection2 :: Char,
    foodPos :: Coord,
    menu :: Bool,
    paused :: Bool,
    difficulty :: Int, 
    framecount :: Int,
    time :: Float,
    gameOver :: Bool,
    score :: Float
    }

-- origin coordinate for reference
origin :: Coord
origin = (0,0)

-- initial state of game
initSnake :: Game
initSnake = Game
  { headPos = origin,
    prevPos = [],
    bodyLength = 5, 
    moveDirection = 'd',
    foodPos = (250,0),
    menu = True,
    paused = False,
    difficulty = 2,
    framecount = 0,
    time = 0,
    gameOver = False,
    score = 0
    }

-- coordinate x and y for reference
type Coord = (Float, Float)

-- unit of measurement in gui
unit :: Float
unit = 10

-- gui bg colour    
bgcol :: Color
bgcol = makeColor 0.17 0.18 0.20 1

-- gui window
glossGUI :: Display
glossGUI = InWindow "Snake Game" (750, 750) (0,0)

xHead :: Game -> Float
xHead game = fst(headPos game)

yHead :: Game -> Float
yHead game = snd(headPos game)

-- render the game states
render :: Game -> Picture
render game
  | (gameOver game) =
    pictures [translate (-200) 200    $ scale 0.5 0.5   $ color white $ text "Game Over",
              translate (-200) (-100) $ scale 0.2 0.2   $ color white $ text "R to try again!",
              translate (-200) (-200) $ scale 0.2 0.2   $ color white $ text "X to return to menu!"]
  | (menu game) = 
    pictures [translate (-200) 200    $ scale 1 1       $ color white $ text "Snake",
              translate (-200) 100    $ scale 0.2 0.2   $ color white $ text "Press Enter to play",
              translate (-200) 50     $ scale 0.2 0.2   $ color white $ text "Difficulty:",
              translate (-200) 25     $ scale 0.15 0.15 $ color (colorSelector game 3) $ text "[1] Easy",
              translate (-200) 0      $ scale 0.15 0.15 $ color (colorSelector game 2) $ text "[2] Medium",
              translate (-200) (-25)  $ scale 0.15 0.15 $ color (colorSelector game 1) $ text "[3] Hard",
              translate (-190) (-95)  $ scale 0.13 0.13   $ color white $ text "W",
              translate (-200) (-112) $ scale 0.13 0.13   $ color white $ text "ASD",
              translate (-156) (-105) $ scale 0.13 0.13   $ color white $ text "for movement",
              translate (-200) (-145) $ scale 0.13 0.13   $ color white $ text "R to restart"]
  | otherwise = 
    pictures [snakeHead, body, food, score, clock, xText, yText, foodText]
      where
        snakeHead = uncurry translate (headPos game) $ color green $ rectangleSolid 10 10
        body = pictures (renderBody (prevPos game) (bodyLength game))
        food = uncurry translate (foodPos game) $ color red $ rectangleSolid 10 10
      -- scores
        scoreDisplay = show (bodyLength game)
        score = translate (-200) 200 $ scale 0.5 0.5 $ color white $ text scoreDisplay
      -- time	
        timeDisplay = show (time game)
        clock    = translate 375 375 $ scale 0.25 0.25 $ color white $ text timeDisplay
        xText    = translate 200 200 $ scale 0.15 0.15 $ color white $ text (show (xHead game))
        yText    = translate 200 180 $ scale 0.15 0.15 $ color white $ text (show (yHead game))
        foodText = translate 150 150 $ scale 0.15 0.15 $ color white $ text (show (foodPos game))

colorSelector :: Game -> Int -> Color
colorSelector game dif
  | (difficulty game) == dif = yellow
  | otherwise = white
-- render snake body, no coords or body length = 0 results in an empty list; no snake
renderBody :: [Coord] -> Float -> [Picture]
renderBody [] _ = []
renderBody _ 0 = []
renderBody (x:xs) temp
  | temp > 0 = (uncurry translate x $ color green $ rectangleSolid 10 10) : renderBody xs (temp-1)
  
-- movement -> direction + unit(10)
move :: Coord -> Char -> Coord
move pos direction
  | direction == 'w' = (fst(pos),snd(pos) + unit)
  | direction == 'a' = (fst(pos) - unit,snd(pos))
  | direction == 's' = (fst(pos),snd(pos) - unit)
  | direction == 'd' = (fst(pos) + unit,snd(pos))
  | otherwise = (pos)
-- controls
controls :: Event -> Game -> Game

-- movement
controls (EventKey (Char 'w') Down _ _) game 
  | (moveDirection game) /= 's' = game {moveDirection = 'w'}
  | otherwise = game
controls (EventKey (Char 'a') Down _ _) game
  | (moveDirection game) /= 'd' = game {moveDirection = 'a'}
  | otherwise = game
controls (EventKey (Char 's') Down _ _) game
  | (moveDirection game) /= 'w' = game {moveDirection = 's'}
  | otherwise = game
controls (EventKey (Char 'd') Down _ _) game
  | (moveDirection game) /= 'a' = game {moveDirection = 'd'}
  | otherwise = game

-- restart
controls (EventKey (Char 'r') _ _ _) game = initSnake

-- menu selectors
controls (EventKey (Char '1') _ _ _) game
  | (menu game) = game {difficulty = 3}
  | otherwise = game
controls (EventKey (Char '2') _ _ _) game
  | (menu game) = game {difficulty = 2}
  | otherwise = game
controls (EventKey (Char '3') _ _ _) game
  | (menu game) = game {difficulty = 1}
  | otherwise = game
controls (EventKey (SpecialKey KeyEnter) _ _ _) game 
  | (menu game) = game {menu = False}
  | otherwise = game
controls (EventKey (SpecialKey KeyPadEnter) _ _ _) game 
  | (menu game) = game {menu = False}
  | otherwise = game
-- -- pause/unpause
--controls (EventKey (Char ' ') _ _ _) game 
--  | game = game {paused = True}
--  | otherwise = game
-- gameOver screen restart option
controls (EventKey (Char 'x') _ _ _) game
  | (gameOver game) = initSnake
  | otherwise = game
--controls (EventKey (Char 'h') _ _ _) game
--  | (gameOver game) = game (menu = True)
--  | otherwise game
-- disable other keys
controls _ game = game -- other keys do nothing

-- animates the movement/length of the snake, addition of times and moves the apple once eaten
renderAnimate :: Float -> Game -> Game
renderAnimate seconds game
  | (menu game) || (gameOver game) = game
  | (framecount game) == 0 = 
                game { headPos    = nextHeadPos,
                       prevPos    = lastHeadPos,
                       bodyLength = newLength,
                       foodPos    = nextFoodPos,
                       framecount = nextFrame,
                       time       = newTime,
                       gameOver   = checkGO,
                       score      = newScore}
  -- game just started , we initialize all our variables that we need to change per frame
  | otherwise =  game{time = newTime, framecount = nextFrame}
                where
                  headPosBefore    = (headPos game)
                  bodyLengthBefore = (bodyLength game)
                  foodPosBefore    = (foodPos game)
                  frameBefore      = (framecount game)
                  oldTime          = (time game)
                  nextHeadPos      = move headPosBefore (moveDirection game)
                  lastHeadPos      = headPosBefore : (prevPos game)
                  newLength        = growth headPosBefore (foodPos game) bodyLengthBefore
                  newScore         = calcScore bodyLengthBefore
                  nextFoodPos      = genNewFood headPosBefore foodPosBefore
                  nextFrame        = mod (1 + frameBefore) (difficulty game) -- placeholder for now lemao
                  newTime          = oldTime + seconds
                  checkGO          = (isGameOver game)

-- increment bodyLength by one when eating food
growth :: Coord -> Coord -> Float -> Float 
growth headPos foodPos bodyLength
  | headPos == foodPos = bodyLength + 1
  | otherwise = bodyLength
  
-- new food location if head has touched the food
genNewFood :: Coord -> Coord -> Coord
genNewFood a b = 
  if a == b then multiplier (rounder (unsafePerformIO newFoodCoord))
  else b

newFoodCoord :: IO Coord
newFoodCoord = 
      do
        n1 <- randomRIO (-37, 37)
        n2 <- randomRIO (-37, 37)
        return (n1,n2)

multiplier :: Coord -> Coord
multiplier (x,y) = (x*10, y*10)

rounder :: Coord -> Coord
rounder (x,y) = (toCoord (toInte x, toInte y))
  
toInt :: Float -> Int
toInt = round

toInte :: Float -> Integer
toInte = round

toCoord :: (Integer, Integer) -> (Float, Float)
toCoord (x,y) = (fromIntegral x, fromIntegral y)

-- checks if game is over, either the snake head hits the wall, or collides with itself
isGameOver :: Game -> Bool
isGameOver game
  | fst(headPos game) == 370 || fst(headPos game) == -370 || snd(headPos game) == 370 || snd(headPos game) == -370 = True
  | checkSelfCollision (bodyLength game) (headPos game) (prevPos game) = True
--  | checkSnakeOneCollision (bodyLength game) (bodyLength2 game) (headPos game) (headPos2 game) (prevPos game) (prevPos2 game) = True
--  | checkSnakeTwoCollision (bodyLength game) (bodyLength2 game) (headPos game) (headPos2 game) (prevPos game) (prevPos2 game) = True
  | otherwise = False
  
-- checks if the snake is self colliding
checkSelfCollision :: Float -> Coord -> [Coord] -> Bool
checkSelfCollision _ _ [] = False
checkSelfCollision bodyLength headPos prevPos = elem headPos (take (toInt bodyLength) prevPos)

-- create second snake
--renderBodyTwo :: [Coord] -> Integer -> []
--renderBodyTwo [] _ = []
--renderBodyTwo _ 0 = []
--renderBodyTwo (x:xs) temp
--  | temp > 0 = (uncurry translate x $ color green $ rectangleSolid 10 10) : renderBodyTwo xs (temp-1)
-- check if snake 1 has collided with snake 2 (Snake 2 wins)
-- check if snake 2 has collided with snake 1 (Snake 1 wins)
-- check if snake has collided with wall (other snake wins)

-- scoring system
calcScore :: Float -> Float
calcScore n = n - 5

-- StatesPerSecond
sps :: Int
sps = 100
