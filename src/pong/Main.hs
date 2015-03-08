

module Main (main) where

import Pong.Constants
import Pong.Player
import Pong.View
import Pong.World

import Graphics.Gloss.Interface.Pure.Game

initialWorld :: World
initialWorld = World { field = 0, leftPlayer = Player { xPos = 10 - (screenWidthF / 2), yPos = 10 } }

step :: Float -> World -> World
step elapsed w@(World { field = fieldValue }) = w { field = fieldValue + elapsed }

react :: Event -> World -> World
react (EventMotion (_, mouseYPos)) world@(World { leftPlayer = playerValue })
  = world { leftPlayer = playerValue { yPos = mouseYPos } }
react _ w = w

render :: World -> Picture
render world = toPicture world

main :: IO ()
main = do
  let world = initialWorld
  play (InWindow "Pong" (screenWidth, screenHeight) (10, 10))
    white 30 world render react step

