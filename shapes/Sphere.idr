module Sphere

import Hittable
import Vec3
import Ray

%access export

public export

record Sphere where
    constructor MkSphere
    center : Vec3
    radius : Double

Hittable Sphere where
    hit sphere ray t_min t_max = 
       if discriminant > 0 
            then
                if temp < t_max && temp > t_min
                    then set_face_normal ray oNormal rec
                    else 
                        if temp' < t_max && temp' > t_min
                            then set_face_normal ray oNormal' rec'
                            else MkHitRecord (0,0,0) (0,0,0) temp' False False
            else MkHitRecord (0,0,0) (0,0,0) 1 False False
    where
        oc : Vec3
        oc           = (orig ray) - center sphere
        a : Double
        a            = lengthSquared (dir ray)
        half_b : Double
        half_b       = dot oc (dir ray)
        c : Double
        c            = (lengthSquared oc) - radius sphere * radius sphere
        discriminant : Double
        discriminant = half_b*half_b - a*c

        root : Double
        root = sqrt discriminant
        temp : Double
        temp = (-half_b - root) / a
        temp' : Double
        temp' = (-half_b + root) / a

        rec_p : Vec3
        rec_p   = at ray temp
        rec : HitRecord
        rec     = MkHitRecord rec_p (0, 0, 0) temp False True
        oNormal : Vec3
        oNormal = (rec_p - center sphere) / radius sphere

        rec_p' : Vec3
        rec_p'   = at ray temp'
        rec' : HitRecord
        rec'     = MkHitRecord rec_p (0, 0, 0) temp' False True
        oNormal' : Vec3
        oNormal' = (rec_p' - center sphere) / radius sphere
                       