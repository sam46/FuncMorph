{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}  


module FMorph (
        linspace, remap, lerpPts, 
        rotatePts, squarify, randPts, 
        squiggly, squigglify, fuzzify, 
        project2D, scalePts, singlePoint,
        project3D, rotateX, rotateY, rotateZ,
        kock, regularKock, regular, polar,
        samplePts, splitapart, plane,
        draw, drawShape, play
    ) where 

import Data.Active
import Diagrams.Prelude
import Diagrams.Backend.Cairo
import Diagrams.Animation
import Data.Tuple
import System.Random
import System.Random.Shuffle
import Data.List (sort)


---- Constants ----
singlePoint = circle 0.9 # fc red
baseSpeed = 0.3


---- Transformations and effects ----

-- sample a list (stable, i.e. order is preserved)
samplePts points n rgen = map (points !!) (sort $ take n shuff)
        where shuff = (shuffle' [0..length points -1] (length points) rgen)

scalePts r points = map (unr2.(*r).r2) points
project2D scalars = zip scalars $ repeat 0.0
project3D points  = map (\(a, b)-> (a, b, 0.0)) points

linspace num = map (/num) [0..num]
linspace num = map (/num) [0..num]

-- rectangular grid of nPts^2 points
plane nPts = let ls = linspace nPts in [(x,y) | x <- ls, y <- ls]


remap x a b c d             = fst $ unr2 $ 
                                lerp ((x - a)/(b - a)) (r2 (d,0)) (r2 (c,0))
lerpPts amt points1 points2 = map unr2 $ 
                                zipWith (lerp amt) (map r2 points2) (map r2 points1)


rotate'   ang (x,y)  = ((cos ang)*x - (sin ang)*y, (sin ang)*x + (cos ang)*y)
rotatePts ang points = map (rotate' ang) points
rotateX   ang points = map (\(x,y,z)-> let (ry,rz) = rotate' ang (y,z) 
                                        in (x,ry,rz)) points
rotateY   ang points = map (\(x,y,z)-> let (rx,rz) = rotate' ang (x,z) 
                                        in (rx,y,rz)) points
rotateZ   ang points = map (\(x,y,z)-> let (ry,rx) = rotate' ang (y,x) 
                                        in (rx,ry,z)) points


foo small' big' = ((remap (abs small') (-c') c' (-side) side) * signum small', 
                   side * signum big')
    where   c' = 100*(cos (pi/4.0))
            side = 100
squarify'(x,y)  =  if abs x < abs y then foo x y else swap $ foo y x
squarify points = map squarify' points

squigglify' rad' nSquiggles amp x = (radS*cos(2*pi*x), radS*sin(2*pi*x))
    where   radS = rad' + amp*(cos(nSquiggles*ang)) -- radius dependent on angle
            ang   = 2*pi*x
squigglify rad' nSquiggles amp points = map (squigglify' rad' nSquiggles amp) points
squiggly   rad' nSquiggles amp numPts = squigglify rad' nSquiggles amp $ 
                                        linspace numPts

randScalars n rgen = take n $ randomRs (0,1) rgen :: [Double]
randPts     n rgen = zip (take n $ randScalars (2*n::Int) rgen) 
                         (drop n $ randScalars (2*n::Int) rgen) 

fuzzify' xamp yamp (x,y) (rx,ry) = (x + xamp*rx, y + yamp*ry)
fuzzifyR xamp yamp rands points  = zipWith (fuzzify' xamp yamp) points rands 
fuzzify  xamp yamp rgen  points  = fuzzifyR xamp yamp (randPts (length points) rgen) points 

kock' :: (Floating a) => V2 a -> V2 a -> Int -> a -> [V2 a]
-- kock fractal given two pivot points (line-segment) and number of iterations (levels).
-- length of list is dependent on n: length (kock' _ _ n _) == 5 * (4**n) 
-- v1, v2: segment endpoints
-- n: iterations (levels)
-- tipLen: how far the tips pop out
kock' v1 v2 n tipLen =
    let  v12   = v2 - v1
         v12'  = v1 + v12 / 3 
         v12'' = v1 + 2 * v12 / 3 
         tip   = (v1 + v2) / 2 + (perp v12) ^* (tipLen / norm v12) in 
    if (n < 0) then [] 
    else if (n==0) then [v1, v12', tip, v12'', v2] 
    else (kock' v1    v12'  (n-1) (tipLen*0.2)) ++  
         (kock' v12'  tip   (n-1) (tipLen*0.2)) ++
         (kock' tip   v12'' (n-1) (tipLen*0.2)) ++
         (kock' v12'' v2    (n-1) (tipLen*0.2)) 

-- kock fractal given two pivot points (line-segment). 
-- Sampled to only use nPts points: i.e. length (kock' _ _ nPts _ _) == nPts
kock pt1 pt2 tipLen nPts rgen = samplePts kockPts nPts rgen
    where nPts'   = ceiling $ log ((fromIntegral nPts)/5) / log 4 
          toPair  = \v -> (v ^. _x, v ^. _y)
          kockPts = map toPair $ kock' (uncurry V2 pt1) (uncurry V2 pt2) nPts' tipLen

-- regular polygon with sides replaced with kock fractals
-- Sampled to only use nPts points: i.e. length (regularKock _ nPts _ _ _) == nPts
regularKock n nPts tipLen rad' rgen = 
    concatMap (\(v1, v2) -> 
        kock v2 v1 tipLen (ceiling $ (fromIntegral nPts) / (fromIntegral n)) rgen
        ) segments
    where regVerts = scalePts rad' $ regular n
          segments = zip regVerts (tail regVerts ++ [head regVerts])  

-- point on a unit circle given an angle
polar   t  = (cos t, sin t)

-- equaly sperated points on a unit circle
regular n  = take n $ map polar [0, tau / fromIntegral n .. ]


-- pull points apart
splitapart  _            0 points = points
splitapart  (gapX, gapY) n points =
    splitapart (gapX/2, gapY/2) (n-1) (map (\(x,y) -> (x-gapX, y-gapY)) lower) ++
    splitapart (gapX/2, gapY/2) (n-1) (map (\(x,y) -> (x+gapX, y+gapY)) upper)
    where lower = (take (floor (fromIntegral (length points)/2)) points)
          upper = (drop (floor (fromIntegral (length points)/2)) points)

---- Animation/Drawing  ----
lerpShots   t (shotx, shoty) = draw $ lerpPts t (map twoDim shotx) (map twoDim shoty)
mkScene speed shotPair = ((pure $ (flip lerpShots) shotPair) <*> stretch (baseSpeed*speed) ui)
                               :: Animation B V2 Double

class TwoDim a b where
    twoDim :: a b -> (b, b)

instance TwoDim ((,) a) a where
    twoDim = id

instance TwoDim ((,,) a a) a where
    twoDim (x, y, _) = (x, y) 


-- make a Digaram from a shot where a shot is a list of points (i.e. [(x,y)] or [(x,y,z)]) 
drawShape ptShape points  = atPoints (map (p2.twoDim) points) $ repeat ptShape
draw                      = drawShape singlePoint . map twoDim

getShots  xs = map fst xs
getSpeeds xs = map snd xs

-- make an animation given [(shot,time)] 
-- where shot is a list of points (i.e. [(x,y)] or [(x,y,z)]) 
-- and time is a Num
play      xs = movie $ zipWith ($)
            (map mkScene $ getSpeeds $ init xs)
            $ zip (getShots $ init xs) (getShots $ tail xs)
