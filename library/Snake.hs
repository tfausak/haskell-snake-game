module Snake where

import qualified Graphics.Gloss.Interface.Pure.Game as G

main :: IO ()
main = G.play
    displayMode
    backgroundColor
    stepRate
    initialWorld
    drawWorld
    handleEvent
    handleStep

--

displayMode :: G.Display
displayMode = G.InWindow "Snake" (640, 480) (0, 0)

backgroundColor :: G.Color
backgroundColor = G.white

stepRate :: Int
stepRate = 1

initialWorld :: World
initialWorld = NewWorld

drawWorld :: World -> G.Picture
drawWorld _world = G.blank

handleEvent :: G.Event -> World -> World
handleEvent _event world = world

handleStep :: Float -> World -> World
handleStep _time world = world

--

data World = NewWorld
    {
    } deriving (Eq, Ord, Read, Show)
