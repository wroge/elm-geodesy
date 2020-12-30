module Geodesy.Transformation exposing (..)

import Geodesy.Coordinate


type alias Transformation =
    { toWGS84 : Geodesy.Coordinate.Coordinate -> Geodesy.Coordinate.Coordinate
    , fromWGS84 : Geodesy.Coordinate.Coordinate -> Geodesy.Coordinate.Coordinate
    }
