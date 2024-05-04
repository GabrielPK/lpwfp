{-# OPTIONS -Wall #-}

module Newton2 where

import Graphics.Gnuplot.Simple

type R = Double
type Mass = R
type Time = R
type Position = R
type Velocity = R
type Force = R

velocityCF :: Mass 
  -> Velocity 
  -> [Force] 
  -> (Time -> Velocity)
velocityCF m v0 fs = 
  let fNet = sum fs --net force
      a0 = fNet / m --acceleration
      v t = v0 + a0 * t -- constant acceleration eqn
  in v

positionCF :: Mass
  -> Position
  -> Velocity
  -> [Force]
  -> (Time -> Position)
positionCF m x0 v0 fs = 
  let fNet = sum fs
      a0 = fNet / m 
      x t = x0 + v0 * t + a0*t**2 / 2
  in x

