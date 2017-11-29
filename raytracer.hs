module Raytracer where

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
  
cross :: Vector3 -> Vector3 -> Vector3
cross a b =
  let (a1, a2, a3) = a
      (b1, b2, b3) = b in
  ((a2 * b3) - (a3 * b2),
   (a3 * b1) - (a1 * b3),
   (a1 * b2) - (a2 * b1))

type Line = (Vector3, Vector3)
type Sphere = (Vector3, Float)
type SphereIntersection = (Float, Float)

descriminant :: Float -> Float -> Float -> Float
descriminant a b c =
  b ^ 2 - 4 * a * c

lineIntersectSphere :: Line -> Sphere -> Maybe SphereIntersection
lineIntersectSphere line sphere =
  let ((p1, p2, p3), (d1, d2, d3)) = line
      ((s1, s2, s3), r) = sphere
      a = d1 ^ 2 + d2 ^ 2 + d3 ^ 2
      b = 2 * (d1 * (s1 + p1) + d2 * (s2 + p2) + d3 * (s3 + p3))
      c = s1 ^ 2 + s2 ^ 2 + s3 ^ 2 - r ^ 2
      d = descriminant a b c in
      if d > 0 then
        let t1 = ((sqrt d) - b) / (2 * a)
            t2 = ((sqrt d) + b) / (2 * a) in
        Just (t1, t2)
      else
        Nothing
