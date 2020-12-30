module Geodesy.Projection exposing (..)

import Geodesy.Coordinate
import Geodesy.Spheroid
import Geodesy.Utils


type alias Projection =
    { toGeographic : Geodesy.Spheroid.Spheroid -> Geodesy.Coordinate.Coordinate -> Geodesy.Coordinate.Coordinate
    , fromGeographic : Geodesy.Spheroid.Spheroid -> Geodesy.Coordinate.Coordinate -> Geodesy.Coordinate.Coordinate
    }


webMercator : Projection
webMercator =
    { toGeographic =
        \spheroid ( east, north, h ) ->
            ( Geodesy.Utils.degree (east / spheroid.a)
            , atan (e ^ (north / spheroid.a)) * Geodesy.Utils.degree 1 * 2 - 90
            , h
            )
    , fromGeographic =
        \spheroid ( lon, lat, h ) ->
            ( Geodesy.Utils.radian lon * spheroid.a
            , logBase e (tan (Geodesy.Utils.radian (90 + lat) / 2)) * spheroid.a
            , h
            )
    }
