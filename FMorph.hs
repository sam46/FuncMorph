{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module FMorph (
        fmWidth, fmHeight, nPts, fmRad,
        linspace, lerpScaler, lerpPts, 
        rotatePts, squarify, randPts, 
        squigglify, fuzzify,
        draw, play
    ) where 

import Data.Active
import Diagrams.Prelude
import Diagrams.Backend.Cairo
import Diagrams.Animation
import Data.Tuple
import System.Random


--- Constants ---
nPts :: Int
nPts = 100
fmWidth = 100.0
fmHeight = 100.0
singlePoint = circle 1 # fc black
baseSpeed = 0.3
fmRad = 100


--- Transformations and effects ---
linspace num = map (/num) [0..num]
-- std = linspace nPts

lerpScaler x a b c d      = fst $ unr2 $ 
                                lerp ((x - a)/(b - a)) (r2 (d,0)) (r2 (c,0))
lerpPts amt points1 points2 = map unr2 $ 
                                zipWith (lerp amt) (map r2 points2) (map r2 points1)

rotate'   ang (x,y)  = ((cos ang)*x - (sin ang)*y, (sin ang)*x + (cos ang)*y)
rotatePts ang points = map (rotate' ang) points

foo small' big' = ((lerpScaler (abs small') (-c') c' (-side) side) * signum small', 
                   side * signum big')
    where   c' = 100*(cos (pi/4.0))
            side = 100
squarify'(x,y)  =  if abs x < abs y then foo x y else swap $ foo y x
squarify points = map squarify' points

squigglify' num amp x = (rad'*cos(2*pi*x), rad'*sin(2*pi*x))
    where   rad' = fmRad + amp*(cos(num*ang)) -- radius dependent on angle
            ang = 2*pi*x
squigglify num amp points = map (squigglify' num amp) points

randScalars n rgen = take n $ randomRs (0,1) rgen :: [Double]
randPts     n rgen = zip (take n $ randScalars (2*n::Int) rgen) 
                         (drop n $ randScalars (2*n::Int) rgen) 

fuzzify' xamp yamp (x,y) (rx,ry) = (x + xamp*rx, y + yamp*ry)
fuzzifyR xamp yamp rands points  = zipWith (fuzzify' xamp yamp) points rands 
fuzzify  xamp yamp rgen  points  = fuzzifyR xamp yamp (randPts (length points) rgen) points 


---   ---
lerpShots   t (shotx, shoty) = draw $ lerpPts t shotx shoty
mkScene speed shotPair = ((pure $ (flip lerpShots) shotPair) <*> stretch (baseSpeed*speed) ui)
                               :: Animation B V2 Double

draw  points = atPoints (map p2 points) $ repeat singlePoint

getShots  xs = map fst xs
getSpeeds xs = map snd xs

play      xs = movie $ zipWith ($)
            (map mkScene $ getSpeeds $ init xs)
            $ zip (getShots $ init xs) (getShots $ tail xs)
