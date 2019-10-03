module Vector exposing (Vector, difference, distance, divide, magnitude, sum, times)


type alias Vector =
    { x : Float
    , y : Float
    }


divide : Vector -> Float -> Vector
divide vec scalar =
    times vec (1 / scalar)


times : Vector -> Float -> Vector
times vec scalar =
    { x = vec.x * scalar
    , y = vec.y * scalar
    }


sum : Vector -> Vector -> Vector
sum a b =
    { x = a.x + b.x
    , y = a.y + b.y
    }


distance : Vector -> Vector -> Float
distance a b =
    (b.x - a.x) ^ 2 + (b.y - a.y) ^ 2 |> sqrt


magnitude : Vector -> Float
magnitude =
    distance (Vector 0 0)


difference : Vector -> Vector -> Vector
difference v1 v2 =
    { x = v2.x - v1.x
    , y = v2.y - v1.y
    }
