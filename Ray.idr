module Ray

import Vec3

%access export

public export

record Ray where
    constructor MkRay
    orig, dir : Vec3

at : Ray -> Double -> Vec3
at ray t = orig ray + t * dir ray

hitSphere : Vec3 -> Double -> Ray -> Bool
hitSphere center radius ray = discriminant > 0
            where 
                oc : Vec3
                oc = orig ray - center

                a : Double
                a = dot (dir ray) (dir ray)

                b : Double
                b = 2.0 * dot oc (dir ray)

                c : Double
                c = dot oc oc - radius * radius

                discriminant : Double
                discriminant = b*b - 4*a*c

rayColor : Ray -> Vec3
rayColor ray = if hitSphere (0.0, 0.0, -1.0) 0.5 ray
    then
        (1.0, 0.0, 0.0)
    else
        (1.0 - t) * (1.0, 1.0, 1.0) + t * (0.5, 0.7, 1.0)
        where 
            direction : Vec3
            direction = unitVector (dir ray)
            t         = 0.5 * (getY direction + 1.0) 
