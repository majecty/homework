

module Main (main) where

import Graphics.Gloss.Interface.Pure.Game
import Data.Monoid

data Player = Player { xPos :: Float, yPos :: Float }

data World = World { field :: Float,
    leftPlayer :: Player
  }

playerWidth :: Int
playerWidth = 40

playerHeight :: Int
playerHeight = 80

screenWidth :: Int
screenWidth = 400

screenWidthF :: Float
screenWidthF = 400

screenHeight :: Int
screenHeight = 300

screenHeightF :: Float
screenHeightF = 300

initialWorld :: World
initialWorld = World { field = 0, leftPlayer = Player { xPos = 10, yPos = 10 } }

step :: Float -> World -> World
step elapsed w@(World { field = fieldValue }) = w { field = fieldValue + elapsed }

react :: Event -> World -> World
react ev w@(World { field = fieldValue }) = w

render :: World -> Picture
render (World { field = fieldValue, leftPlayer = Player { xPos = xPosValue, yPos = yPosValue } })
  = translate (-screenWidthF / 2) (-screenHeightF / 2) $
    (translate (screenWidthF / 2) (screenHeightF / 2) $ Circle fieldValue)
    <> (translate xPosValue yPosValue $ polygon playerPath)
  where
    playerPath = [(0,0), (fromIntegral playerWidth, 0),
      (fromIntegral playerWidth, fromIntegral playerHeight), (0, fromIntegral playerHeight)]

main :: IO ()
main = do
  let world = initialWorld
  play (InWindow "Pong" (screenWidth, screenHeight) (10, 10))
    white 30 world render react step

