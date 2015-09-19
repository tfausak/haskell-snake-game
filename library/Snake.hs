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
    { resolution = (640, 480)
    }

drawWorld :: World -> G.Picture
drawWorld _world = G.blank

handleEvent :: G.Event -> World -> World
handleEvent event world = case event of
    G.EventResize newResolution -> handleResize newResolution world
    _ -> world

handleStep :: Float -> World -> World
handleStep _time world = world

--

handleResize :: (Int, Int) -> World -> World
handleResize newResolution world = world { resolution = newResolution }

--

data World = NewWorld
    { resolution :: (Int, Int)
    } deriving (Eq, Ord, Read, Show)
