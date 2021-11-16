module Vec3

%access export

public export
-- To RECORD ?
Vec3 : Type
Vec3 = (Double, Double, Double)

toColor : Int -> Int -> Double
toColor value size = 255.999 * (cast value / cast size)

getX : Vec3 -> Double
getX (x, _, _) = x

getY : Vec3 -> Double
getY (_, y, _) = y

getZ : Vec3 -> Double
getZ (_, _, z) = z

(+) : Vec3 -> Vec3 -> Vec3
(+) (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)

(-) : Vec3 -> Vec3 -> Vec3
(-) (x1, y1, z1) (x2, y2, z2) = (x1 - x2, y1 - y2, z1 - z2)

-- (*) : Vec3 -> Vec3 -> Vec3
-- (*) (x1, y1, z1) (x2, y2, z2) = (x1 * x2, y1 * y2, z1 * z2)

(*) : Double -> Vec3 -> Vec3
(*) t (x2, y2, z2) = (t * x2, t * y2, t * z2)

(/) : Vec3 -> Double -> Vec3
(/) v t = (1 / t) * v

dot : Vec3 -> Vec3 -> Double
dot (x1, y1, z1) (x2, y2, z2) = x1 * x2 + y1 * y2 + z1 * z2

length : Vec3 -> Double
length (x, y, z) = sqrt (x*x + y*y + z*z)

lengthSquared : Vec3 -> Double
lengthSquared (x, y, z) = x*x + y*y + z*z

unitVector : Vec3 -> Vec3
unitVector v = v / Vec3.length v

vecToStr : Vec3 -> String
vecToStr (x, y, z) = concat [show $ cast {to=Int} x, " ", show $ cast {to=Int} y, " ", show $ cast {to=Int} z]
