

module Main (main) where

import Pong.Constants
import Pong.Player
import Pong.View

import Graphics.Gloss.Interface.Pure.Game
import Data.Monoid

data World = World { field :: Float,
    leftPlayer :: Player
  }

initialWorld :: World
initialWorld = World { field = 0, leftPlayer = Player { xPos = 10, yPos = 10 } }

step :: Float -> World -> World
step elapsed w@(World { field = fieldValue }) = w { field = fieldValue + elapsed }

react :: Event -> World -> World
react ev w@(World { field = fieldValue }) = w

render :: World -> Picture
render (World { field = fieldValue, leftPlayer = leftPlayerValue })
  = translate (-screenWidthF / 2) (-screenHeightF / 2) $
    (translate (screenWidthF / 2) (screenHeightF / 2) $ Circle fieldValue)
    <> (toPicture leftPlayerValue)

main :: IO ()
main = do
  let world = initialWorld
  play (InWindow "Pong" (screenWidth, screenHeight) (10, 10))
    white 30 world render react step

