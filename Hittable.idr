module Hittable

import Vec3
import Ray
import shapes.Sphere

%access export

public export

record HitRecord where
    constructor MkHitRecord
    p, normal  : Vec3
    t          : Double
    front_face : Bool
    isHitted   : Bool

set_face_normal : Ray -> Vec3 -> HitRecord -> HitRecord
set_face_normal ray outward_normal rec = 
    record { 
        -- front_face = dot (dir ray) outward_normal < 0.0 
        normal = outward_normal
            -- then outward_normal 
            -- else -outward_normal
        } rec

public export

interface Hittable s where
    hit : Ray -> Double -> Double -> s -> HitRecord

data HittableList = List (Sphere)

Hittable HittableList where
    hit ray t_min t_max list = 
        map (hit ray t_min t_max) list
