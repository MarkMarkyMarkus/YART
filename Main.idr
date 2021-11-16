module Main

import Vec3
import Ray
-- import shapes.Sphere
import Hittable

aspectRatio : Double
aspectRatio = 16.0 / 9.0

width : Int
width = 400

height : Int
height = cast (cast width / aspectRatio)

-- World
world : List (Sphere)
world = [
    MkSphere (0, 0, -1) 0.5, 
    MkSphere (0, -100.5, -1) 100
]

viewport_height : Double
viewport_height = 2.0

viewport_width : Double
viewport_width = aspectRatio * viewport_height

focalLength : Double
focalLength = 1.0

origin : Vec3
origin = (0.0, 0.0, 0.0)

horizontal : Vec3
horizontal = (viewport_width, 0.0, 0.0)

vertical : Vec3
vertical = (0.0, viewport_height, 0.0)

lower_left_corner : Vec3
lower_left_corner = origin - horizontal/2 - vertical/2 - (0.0, 0.0, focalLength)

rayColor : Ray -> HittableList -> Vec3
rayColor ray world = 
        let hit' = hit world ray 0.0 99999
        in 
            if isHitted hit'
                then
                    0.5 * (normal hit' + (1, 1, 1))
                else
                    (1.0 - t) * (1.0, 1.0, 1.0) + t * (0.5, 0.7, 1.0)
                    where 
                        direction : Vec3
                        direction = unitVector (dir ray)
                        t         = 0.5 * (getY direction + 1.0)

pixelColor : Int -> Int -> Vec3
pixelColor h w = 
    rayColor (MkRay origin (lower_left_corner + (cast h / (cast width-1)) * horizontal + (cast w / (cast height-1)) * vertical - origin)) Main.world

writeColor : Vec3 -> Vec3
writeColor v = 255.999 * v

-- //TODO: seg fault !!!
createImg : Int -> Int -> List (Vec3)
createImg w h = [writeColor $ pixelColor j i | i <- [(h - 1)..0], j <- [1..w]]

main : IO ()
main = do putStrLn $ concat ["P3\n", show width, " ", show height, "\n255\n"]
          traverse putStrLn (map vecToStr $ createImg width height)
          pure ()
