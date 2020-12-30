module Geodesy.Utils exposing (..)


degree : Float -> Float
degree rad =
    rad * 180 / pi


radian : Float -> Float
radian deg =
    deg * pi / 180
