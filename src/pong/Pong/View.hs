
module Pong.View where

import Pong.Constants
import Pong.Player
import Pong.World

import Data.Monoid

import Graphics.Gloss.Interface.Pure.Game

class Renderable a where
  toPicture :: a -> Picture

instance Renderable World where
  toPicture (World { field = fieldValue, leftPlayer = leftPlayerValue })
    = translate (-screenWidthF / 2) (-screenHeightF / 2) $
      (translate (screenWidthF / 2) (screenHeightF / 2) $ Circle fieldValue)
      <> (toPicture leftPlayerValue)

instance Renderable Player where
  toPicture Player { xPos = xPosValue, yPos = yPosValue }
    = (translate xPosValue yPosValue $ polygon playerPath)
    where
      playerPath = [(0,0), (fromIntegral playerWidth, 0),
        (fromIntegral playerWidth, fromIntegral playerHeight), (0, fromIntegral playerHeight)]
