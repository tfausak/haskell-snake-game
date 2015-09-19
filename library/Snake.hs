module Snake where

import qualified Graphics.Gloss.Interface.Pure.Game as G
import qualified System.Random as R

main :: IO ()
main = do
    seed <- R.randomIO
    let world = initialWorld seed

    G.play
        (displayMode world)
        backgroundColor
        stepRate
        world
        drawWorld
        handleEvent
        handleStep

--

displayMode :: World -> G.Display
displayMode world = G.InWindow "Snake" (resolution world) (0, 0)

backgroundColor :: G.Color
backgroundColor = G.white

stepRate :: Int
stepRate = 2

initialWorld :: Int -> World
initialWorld seed = NewWorld
    { resolution = (512, 512)
    , direction = North
    , scale = 11
    , snake = [(0, 2), (0, 1), (0, 0), (0, -1), (0, -2)]
    , isOver = False
    , gen = R.mkStdGen seed
    , food = (2, 2)
    }

drawWorld :: World -> G.Picture
drawWorld world = G.pictures
    [ drawBounds world
    , drawFood world
    , drawSnake world
    , drawGameOver world
    ]

handleEvent :: G.Event -> World -> World
handleEvent event world = case event of
    G.EventResize newResolution -> handleResize newResolution world
    G.EventKey key state _ _ -> if isOver world
        then world
        else handleKey key state world
    _ -> world

handleStep :: Float -> World -> World
handleStep _time world =
    if isOver world
    then world
    else
        let newSnake@((x, y) : _) = init (snake world)
            (x', y') = case direction world of
                North -> (x, y + 1)
                East -> (x + 1, y)
                South -> (x, y - 1)
                West -> (x - 1, y)
        in  if inBounds world (x', y') && not (isSnake world (x', y'))
            then world { snake = (x', y') : newSnake }
            else world { isOver = True }

--

drawBounds :: World -> G.Picture
drawBounds world =
    let x = size world
    in  G.rectangleWire x x

drawFood :: World -> G.Picture
drawFood world = G.color G.green (drawBox (food world) world)

drawSnake :: World -> G.Picture
drawSnake world = G.pictures (map (\ p -> drawBox p world) (snake world))

drawBox :: (Int, Int) -> World -> G.Picture
drawBox (x, y) world =
    let s = size world / fromIntegral (scale world)
        x' = s * fromIntegral x
        y' = s * fromIntegral y
    in  G.translate x' y' (G.rectangleSolid s s)

drawGameOver :: World -> G.Picture
drawGameOver world = if isOver world
    then G.color G.red (G.scale 0.2 0.2 (G.text "game over"))
    else G.blank

--

handleResize :: (Int, Int) -> World -> World
handleResize newResolution world = world { resolution = newResolution }

handleKey :: G.Key -> G.KeyState -> World -> World
handleKey key state world = case state of
    G.Down -> case key of
        G.SpecialKey G.KeyUp ->
            world { direction = if direction world == South then South else North }
        G.SpecialKey G.KeyRight ->
            world { direction = if direction world == West then West else East }
        G.SpecialKey G.KeyDown ->
            world { direction = if direction world == North then North else South }
        G.SpecialKey G.KeyLeft ->
            world { direction = if direction world == East then East else West }
        _ -> world
    _ -> world

--

data World = NewWorld
    { resolution :: (Int, Int)
    , direction :: Direction
    , scale :: Int
    , snake :: [(Int, Int)]
    , isOver :: Bool
    , gen :: R.StdGen
    , food :: (Int, Int)
    } deriving (Read, Show)

size :: (Num a) => World -> a
size world =
    let (width, height) = resolution world
    in  fromIntegral (min width height)

inBounds :: World -> (Int, Int) -> Bool
inBounds world (x, y) =
    let s = scale world `div` 2
    in  -s <= x && x <= s && -s <= y && y <= s

isSnake :: World -> (Int, Int) -> Bool
isSnake world (x, y) = any (== (x, y)) (snake world)

data Direction
    = North
    | East
    | South
    | West
    deriving (Bounded, Enum, Eq, Ord, Read, Show)
