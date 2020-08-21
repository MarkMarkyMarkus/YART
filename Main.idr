module Main

import Vec3
import Ray

aspectRatio : Double
aspectRatio = 16.0 / 9.0

width : Int
width = 400

height : Int
height = cast (cast width / aspectRatio)

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

pixelColor : Int -> Int -> Vec3
pixelColor h w = rayColor $ MkRay origin (lower_left_corner + (cast h / (cast width-1)) * horizontal + (cast w / (cast height-1)) * vertical - origin)

writeColor : Vec3 -> Vec3
writeColor v = 255.999 * v

-- //TODO: seg fault !!!
createImg : Int -> Int -> List (Vec3)
createImg w h = [writeColor $ pixelColor j i | i <- [(h - 1)..0], j <- [1..w]]

main : IO ()
main = do putStrLn $ concat ["P3\n", show width, " ", show height, "\n255\n"]
          traverse putStrLn (map vecToStr $ createImg width height)
          pure ()
