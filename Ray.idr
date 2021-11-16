module Ray

import Vec3
-- import Hittable

%access export

public export

record Ray where
    constructor MkRay
    orig, dir : Vec3

at : Ray -> Double -> Vec3
at ray t = orig ray + t * dir ray

hitSphere : Vec3 -> Double -> Ray -> Double
hitSphere center radius ray = if discriminant < 0 
    then -1.0
    else (-half_b - sqrt discriminant) / a
            where 
                oc : Vec3
                oc = orig ray - center

                a : Double
                a = lengthSquared (dir ray)

                half_b : Double
                half_b = dot oc (dir ray)

                c : Double
                c = lengthSquared oc - radius * radius

                discriminant : Double
                discriminant = half_b*half_b - a*c
