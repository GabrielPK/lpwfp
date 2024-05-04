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

carGraph :: IO ()
carGraph = 
  plotFunc 
    [
      Title "Car on an air track"
    , XLabel "Time (s)"
    , YLabel "Velocity of Car (m/s)"
    , PNG "CarVelocity.png"
    , Key Nothing
    ] [0..4 :: Time] (velocityCF 0.1 0.6 [0.04, -0.08]) 

integral :: R -> (R -> R) -> R -> R -> R
-- integral dt f a b = sum [f t * dt | t <- [a + dt/2, a + 3 * dt/2 .. b - dt/2]]
integral dt f a b = sum $ map (\t -> (f t) * dt) [a + dt/2, a + 3 * dt/2 .. b - dt/2]


antiDerivative :: R -> R -> (R -> R) -> (R -> R)
antiDerivative dt v0 a t = v0 + integral dt a 0 t

velocityFt :: 
  R
  -> Mass
  -> Velocity
  -> [Time -> Force]
  -> (Time -> Velocity)
velocityFt dt m v0 fs = 
  let fNet t = sum $ map (\f -> f t) fs
  -- let fNet t = sum [ f t | f <- fs]
      a t = (fNet t) / m
  in antiDerivative dt v0 a

positionFt :: 
  R 
  -> Mass
  -> Position
  -> Velocity
  -> [Time -> Force]
  -> (Time -> Position)
positionFt dt m x0 v0 fs = 
  antiDerivative dt x0 (velocityFt dt m v0 fs)

fAir :: 
  R -- drag coefficient
  -> R -- air density
  -> R -- cross-sectional area of object
  -> Velocity
  -> Force
fAir drag rho area v = -drag * rho * area * abs v * v / 2

newtonSecondV ::
  Mass
  -> [Velocity -> Force] 
  -> Velocity -- current velocity
  -> R -- derivative of Velocity
newtonSecondV m fs v0 = (/ m) $ sum $ map (\f -> f v0) fs

updateVelocity ::
  R -- time interval dt
  -> Mass 
  -> [Velocity -> Force]
  -> Velocity -- current velocity
  -> Velocity -- new velocity
updateVelocity dt m fs v0 = v0 + (newtonSecondV m fs v0) * dt

velocityFv ::
  R -- time step
  -> Mass
  -> Velocity
  -> [Velocity -> Force]
  -> (Time -> Velocity)
velocityFv dt m v0 fs t =
  let numSteps = abs $ round (t /dt)
  in iterate (updateVelocity dt m fs) v0 !! numSteps

bikeVelocity :: Time -> Velocity
bikeVelocity = velocityFv 1 70 0 [const 100, fAir 2 1.225 0.6]

bikeGraph :: IO ()
bikeGraph = 
  plotFunc
    [ Title "Bike Velocity"
    , XLabel "Time (s)"
    , YLabel "Velocity of Bike (m/s)"
    , PNG "BikeVelocity1.png"
    , Key Nothing 
    ] [0, 0.5..60] bikeVelocity

pedalCoast :: Time -> Force
pedalCoast t = 
  let tCycle = 20
      nComplete :: Int
      nComplete = truncate (t / tCycle)
      remainder = t - fromIntegral nComplete * tCycle
  in if remainder < 10 then 10 else 0

childGraph :: IO ()
childGraph = do
  plotFunc
    [ Title "Child pedaling then coasting"
    , XLabel "Time (s)"
    , YLabel "Position of Bike (m)"
    , PNG "ChildPosition.png"
    , Key Nothing
    ] [0..40 :: R] (positionFt 0.1 20 0 0 [pedalCoast])
