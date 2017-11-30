module Main where

import Graphics.Image as I
import Prelude as P
import GHC.Float

type Vector3 = (Float, Float, Float)

add :: Vector3 -> Vector3 -> Vector3
add a b =
  let (a1, a2, a3) = a
      (b1, b2, b3) = b in
  (a1 + b1, a2 + b2, a3 + b3)

sub :: Vector3 -> Vector3 -> Vector3
sub a b =
  let (a1, a2, a3) = a
      (b1, b2, b3) = b in
  (a1 - b1, a2 - b2, a3 - b3)

mult :: Vector3 -> Float -> Vector3
mult a m =
  let (a1, a2, a3) = a in
  (a1 * m, a2 * m, a3 * m)

unit :: Vector3 -> Vector3
unit a =
  mult a (1 / mag a)

mag :: Vector3 -> Float
mag a =
  let (a1, a2, a3) = a in
  sqrt (a1 ^ 2 + a2 ^ 2 + a3 ^ 2)

dot :: Vector3 -> Vector3 -> Float
dot a b =
  let (a1, a2, a3) = a
      (b1, b2, b3) = b in
  a1 * b1 + a2 * b2 + a3 * b3

type Line = Vector3
type Sphere = (Vector3, Float)
type SphereIntersection = (Float, Float)

descriminant :: Float -> Float -> Float -> Float
descriminant a b c =
  b ^ 2 - (4 * a * c)

lineIntersectSphere :: Line -> Sphere -> Maybe SphereIntersection
lineIntersectSphere line sphere =
  let (r1, r2, r3) = line
      ((s1, s2, s3), r) = sphere
      a = r1 ^ 2 + r2 ^ 2 + r3 ^ 2
      b = 2 * (r1 * s1 + r2 * s2 + r3 * s3)
      c = s1 ^ 2 + s2 ^ 2 + s3 ^ 2 - r ^ 2
      d = descriminant a b c in
      if d > 0 then
        let t1 = ((sqrt d) - b) / (2 * a)
            t2 = ((sqrt d) + b) / (2 * a) in
        Just (t1, t2)
      else
        Nothing

pointToLine :: Int -> Int -> Line
pointToLine i j =
  let r = (-i) + 400
      c = j - 400
      view = ((fromIntegral r), 0, (fromIntegral c))
      cam = (0, -100, 0) in
  sub view cam

traceLineThroughSphere :: Line -> Sphere -> Double
traceLineThroughSphere line sphere =
  case lineIntersectSphere line sphere of
  Nothing -> 1
  Just (t1, t2) ->
    let t = tValue t1 t2
        light = (50, 250, -100)
        lightray = sub light (mult line t)
        (sphereCenter, _) = sphere
        normal = sub line sphereCenter
        dotprod = dot (unit lightray) (unit normal) in
        float2Double dotprod

tValue :: Float -> Float -> Float
tValue t1 t2
  | t1 > 0 && t2 > 0 = min t1 t2
  | t1 > 0 = t1
  | otherwise = t2

tracePointThroughSphere :: Sphere -> (Int, Int) -> Pixel Y Double
tracePointThroughSphere sphere (i, j) =
  PixelY (traceLineThroughSphere (pointToLine i j) sphere)

main :: IO ()
main = do
  let size = 800
      sphere = ((0, 10, 0), 9)
      sphere_image = (makeImageR RPU (size, size) (tracePointThroughSphere sphere))
  writeImage "sphere.png" sphere_image
