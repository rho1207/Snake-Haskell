-- followed steps of http://andrew.gibiansky.com/blog/haskell/haskell-gloss/ which created pong using gloss

module Snake(snake) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random
import Graphics.Gloss.Data.ViewPort

-- PLAY
snake :: IO ()
snake = play glossGUI bgcol fps initSnake render controls renderAnimate

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
    moveDirection = 'e',
    foodPos = (249,0),
    menu = True,
    paused = False,
    difficulty = 0,
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
glossGUI = InWindow "Snake Game" (1000, 1000) (0,0)

-- render the game states
render :: Game -> Picture
render game
  | (gameOver game) =
    pictures [translate (-200) 200    $ scale 1 1       $ color white $ text "Game Over",
              translate (-200) (-200) $ scale 0.5 0.5   $ color white $ text "X to try again!",
              translate (-200) (-300) $ scale 0.5 0.5   $ color white $ text "H to return to menu!"]
  | (menu game) = 
    pictures [translate (-200) 200    $ scale 1 1       $ color white $ text "Snake",
              translate (-200) 100    $ scale 0.2 0.2   $ color white $ text "Press Enter to play",
              translate (-200) 50     $ scale 0.2 0.2   $ color white $ text "Difficulty:",
              translate (-200) 25     $ scale 0.15 0.15 $ color white $ text "[1] Easy",
              translate (-200) 0      $ scale 0.15 0.15 $ color white $ text "[2] Medium",
              translate (-200) (-25)  $ scale 0.15 0.15 $ color white $ text "[3] Hard",
              translate (-200) (-100) $ scale 0.1 0.1   $ color white $ text "'WASD' for movement",
              translate (-200) (-125) $ scale 0.1 0.1   $ color white $ text "'R' to restart",
              translate (-200) (-150) $ scale 0.1 0.1   $ color white $ text "'Spacebar' to pause"]
              
  | otherwise = 
    pictures [snakeHead, body, food, score, clock]
      where
        snakeHead = uncurry translate (headPos game) $ color green $ rectangleSolid 10 10
        body = pictures (renderBody (prevPos game) (bodyLength game))
        food = uncurry translate (foodPos game) $ color red $ circleSolid 10
        
      -- scores
        scoreDisplay = show (bodyLength game)
        score = translate 450 400 $ scale 0.5 0.5 $ color white $ text scoreDisplay
        
      -- time
        timeDisplay = show (time game)
        clock = translate 400 400 $ scale 0.5 0.5 $ color white $ text timeDisplay
    


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
  
-- controls
controls :: Event -> Game -> Game
-- movement
controls (EventKey (Char 'w') _ _ _) game = game {moveDirection = 'w'}
controls (EventKey (Char 'a') _ _ _) game = game {moveDirection = 'a'}
controls (EventKey (Char 's') _ _ _) game = game {moveDirection = 's'}
controls (EventKey (Char 'd') _ _ _) game = game {moveDirection = 'd'}
-- restart
controls (EventKey (Char 'r') _ _ _) game = initSnake
-- menu selectors
controls (EventKey (Char '1') _ _ _) game
  | (menu game) = game {difficulty = 0}
  | otherwise = game
controls (EventKey (Char '2') _ _ _) game
  | (menu game) = game {difficulty = 1}
  | otherwise = game
controls (EventKey (Char '3') _ _ _) game
  | (menu game) = game {difficulty = 2}
  | otherwise = game
controls (EventKey (SpecialKey KeyEnter) _ _ _) game 
  | (menu game) = game {menu = False}
  | otherwise = game
controls (EventKey (SpecialKey KeyPadEnter) _ _ _) game 
  | (menu game) = game {menu = False}
  | otherwise = game
  
-- -- pause/unpause
-- controls (EventKey (Char ' ') _ _ _) game 
--   | game = game {paused = True}
--   | otherwise = game

-- gameOver screen restart option
controls (EventKey (Char 'x') _ _ _) game
  | (gameOver game) = initSnake
  | otherwise = game
--controls (EventKey (Char 'h') _ _ _) game
--  | (gameOver game) = game (menu = True)
--  | otherwise game
-- disable other keys
handleKeys _ game = game -- other keys do nothing

-- animates the movement/length of the snake, addition of times and moves the apple once eaten
renderAnimate :: Float -> Game -> Game
renderAnimate seconds game
  | (menu game) || (gameOver game) = game
  | framecount == 0 = game{headPos = nextHeadPos,
                      prevPos = lastHeadPos,
                      bodyLength = newLength,
                      foodPos = nextFoodPos,
                      framecount = nextFrame,
                      time = newTime,
                      gameOver = checkGO,
                      score = newScore}
  -- game just started , we initialize all our variables that we need to change per frame
  | otherwise =  game{time = newTime, framecount = nextFrame}
                 where
                  headPosBefore = (headPos game)
                  bodyLengthBefore = (bodyLength game)
                  foodPosBefore = (foodPos game)
                  frameBefore = (framecount game)
                  oldTime = (time game)

                  nextHeadPos = move headPosBefore (moveDirection game)
                  lastHeadPos = headPosBefore : (prevPos game)
                  newLength = growth headPosBefore (foodPos game) bodyLengthBefore
                  newScore = calcScore bodyLengthBefore
                  nextFoodPos = genNewFood headPosBefore foodPosBefore
                  nextFrame = 60 + frameBefore -- placeholder for now lemao
                  newTime = oldTime + seconds
                  checkGO = (isGameOver game)

-- STILL NEED TO UPDATE THESE AFTER WRITING HELPERS DONT TOUCH MY SHIT THX

        
-- increment bodyLength by one when eating food
growth :: Coord -> Coord -> Float -> Float 
growth headPos foodPos bodyLength
  | headPos == foodPos = bodyLength + 1
  | otherwise = bodyLength

-- new food location if head has touched the food
genNewFood :: Coord -> Coord -> (Float,Float)
genNewFood a b = 
  if a == b then newFoodCoord
  else b
    
-- generates random coordinates inbounds of -490 to 490 for (x,y)
generateCoords :: IO Float
generateCoords = getStdRandom (randomR (-490,490))

-- generates two integers for use in a coordinate, wraps into tuple
newFoodCoord :: IO (Float, Float) 
newFoodCoord = (,) <$> generateCoords <*> generateCoords

-- checks if game is over, either the snake head hits the wall, or collides with itself
-- add function dec here
isGameOver :: Game -> Bool
isGameOver game
  | fst(headPos game) == 500 || fst(headPos game) == -500 || snd(headPos game) == 500 || snd(headPos game) == -500 = True
  | checkSelfCollision (bodyLength game) (headPos game) (prevPos game) = True
--  | checkSnakeOneCollision (bodyLength game) (bodyLength2 game) (headPos game) (headPos2 game) (prevPos game) (prevPos2 game) = True
--  | checkSnakeTwoCollision (bodyLength game) (bodyLength2 game) (headPos game) (headPos2 game) (prevPos game) (prevPos2 game) = True
  | otherwise = False
  
-- checks if the snake is self colliding
checkSelfCollision :: Float -> Coord -> [Coord] -> Bool
checkSelfCollision _ _ [] = False
checkSelfCollision bodyLength headPos prevPos = elem headPos (take (fromIntegral bodyLength) prevPos)

-- create second snake
--renderBodyTwo :: [Coord] -> Integer -> []
--renderBodyTwo [] _ = []
--renderBodyTwo _ 0 = []
--renderBodyTwo (x:xs) temp
--  | temp > 0 = (uncurry translate x $ color green $ rectangleSolid 10 10) : renderBodyTwo xs (temp-1)

-- check if snake 1 has collided with snake 2 (Snake 2 wins)

-- check if snake 2 has collided with snake 1 (Snake 1 wins)

-- check if snake has collided with wall (other snake wins)
  
-- Updates the game 
update :: ViewPort -> Float -> Game -> Game 
update _ = renderAnimate

-- scoring system
calcScore :: Float -> Float
calcScore n = n - 5

-- i dont think this does what it says it does
fps :: Int
fps = 60
