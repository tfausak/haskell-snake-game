module Snake where

import qualified Graphics.Gloss.Interface.Pure.Game as G

main :: IO ()
main = do
    let world = initialWorld

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
stepRate = 1

initialWorld :: World
initialWorld = NewWorld
    { resolution = (512, 512)
    , direction = North
    }

drawWorld :: World -> G.Picture
drawWorld world = G.pictures
    [ drawBounds world
    ]

handleEvent :: G.Event -> World -> World
handleEvent event world = case event of
    G.EventResize newResolution -> handleResize newResolution world
    G.EventKey key state _ _ -> handleKey key state world
    _ -> world

handleStep :: Float -> World -> World
handleStep _time world = world

--

drawBounds :: World -> G.Picture
drawBounds world =
    let (width, height) = resolution world
        size = fromIntegral (min width height)
    in  G.rectangleWire size size

--

handleResize :: (Int, Int) -> World -> World
handleResize newResolution world = world { resolution = newResolution }

handleKey :: G.Key -> G.KeyState -> World -> World
handleKey key state world = case (key, state) of
    (G.SpecialKey G.KeyUp, G.Down) -> world { direction = North }
    (G.SpecialKey G.KeyRight, G.Down) -> world { direction = East }
    (G.SpecialKey G.KeyDown, G.Down) -> world { direction = South }
    (G.SpecialKey G.KeyLeft, G.Down) -> world { direction = West }
    _ -> world

--

data World = NewWorld
    { resolution :: (Int, Int)
    , direction :: Direction
    } deriving (Eq, Ord, Read, Show)

data Direction
    = North
    | East
    | South
    | West
    deriving (Bounded, Enum, Eq, Ord, Read, Show)
