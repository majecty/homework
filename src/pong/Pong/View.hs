
module Pong.View where

import Pong.Constants
import Pong.Player

import Graphics.Gloss.Interface.Pure.Game

class Renderable a where
  toPicture :: a -> Picture

instance Renderable Player where
  toPicture player@Player { xPos = xPosValue, yPos = yPosValue }
    = (translate xPosValue yPosValue $ polygon playerPath)
    where
      playerPath = [(0,0), (fromIntegral playerWidth, 0),
        (fromIntegral playerWidth, fromIntegral playerHeight), (0, fromIntegral playerHeight)]
