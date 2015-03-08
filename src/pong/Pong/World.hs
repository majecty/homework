
module Pong.World where

import Pong.Player

data World = World { field :: Float,
    leftPlayer :: Player
  }

