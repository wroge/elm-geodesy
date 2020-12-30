module Geodesy.Datum exposing (..)

import Geodesy.Spheroid
import Geodesy.Transformation


type alias Datum =
    { spheroid : Geodesy.Spheroid.Spheroid
    , transformation : Geodesy.Transformation.Transformation
    }
