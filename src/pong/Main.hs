

module Main (main) where

import Graphics.Gloss.Interface.Pure.Game

data World = World { field :: Float }

initialWorld :: World
initialWorld = World { field = 0 }

step :: Float -> World -> World
step elapsed w@(World { field = fieldValue }) = World { field = fieldValue + elapsed }

react :: Event -> World -> World
react ev w@(World { field = fieldValue }) = w

render :: World -> Picture
render (World { field      = fieldValue }) = Circle fieldValue

main :: IO ()
main = do
  let world = initialWorld
  play (InWindow "Pong" (400, 300) (10, 10))
    white 30 world render react step

