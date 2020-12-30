module Geodesy.Spheroid exposing (..)


type alias Spheroid =
    { a : Float, fi : Float }


wgs84 : Spheroid
wgs84 =
    { a = 6378137, fi = 298.257223563 }


a2 : Spheroid -> Float
a2 spheroid =
    spheroid.a ^ 2


f : Spheroid -> Float
f spheroid =
    1 / spheroid.fi


f2 : Spheroid -> Float
f2 spheroid =
    f spheroid ^ 2


b : Spheroid -> Float
b spheroid =
    spheroid.a * (1 - f spheroid)


e2 : Spheroid -> Float
e2 spheroid =
    2 / spheroid.fi - f2 spheroid


e : Spheroid -> Float
e spheroid =
    sqrt (e2 spheroid)


e4 : Spheroid -> Float
e4 spheroid =
    e2 spheroid ^ 2


e6 : Spheroid -> Float
e6 spheroid =
    e2 spheroid ^ 4


ei : Spheroid -> Float
ei spheroid =
    (1 - sqrt (1 - e2 spheroid)) / (1 + sqrt (1 - e2 spheroid))


ei2 : Spheroid -> Float
ei2 spheroid =
    ei spheroid ^ 2


ei3 : Spheroid -> Float
ei3 spheroid =
    ei spheroid ^ 3


ei4 : Spheroid -> Float
ei4 spheroid =
    ei spheroid ^ 4
