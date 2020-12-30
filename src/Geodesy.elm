module Geodesy exposing
    ( CoordinateReferenceSystem
    , CoordinateSystem(..)
    , convert
    , transform
    )

import Geodesy.Coordinate
import Geodesy.Datum
import Geodesy.Projection
import Geodesy.Spheroid
import Geodesy.Transformation
import Geodesy.Utils


type CoordinateSystem
    = Geocentric
    | Geographic
    | Projected Geodesy.Projection.Projection


type alias CoordinateReferenceSystem =
    { coordinateSystem : CoordinateSystem
    , datum : Geodesy.Datum.Datum
    }


transform : CoordinateReferenceSystem -> CoordinateReferenceSystem -> Geodesy.Coordinate.Coordinate -> Geodesy.Coordinate.Coordinate
transform from to ( a, b, c ) =
    fromWGS84 to <| toWGS84 from ( a, b, c )


toWGS84 : CoordinateReferenceSystem -> Geodesy.Coordinate.Coordinate -> Geodesy.Coordinate.Coordinate
toWGS84 from ( a, b, c ) =
    from.datum.transformation.toWGS84 <|
        toGeocentric from.coordinateSystem from.datum.spheroid ( a, b, c )


fromWGS84 : CoordinateReferenceSystem -> Geodesy.Coordinate.Coordinate -> Geodesy.Coordinate.Coordinate
fromWGS84 to ( x, y, z ) =
    fromGeocentric to.coordinateSystem to.datum.spheroid <|
        to.datum.transformation.fromWGS84 ( x, y, z )


convert : CoordinateSystem -> CoordinateSystem -> Geodesy.Spheroid.Spheroid -> Geodesy.Coordinate.Coordinate -> Geodesy.Coordinate.Coordinate
convert from to spheroid ( a, b, c ) =
    fromGeographic to spheroid <| toGeographic from spheroid ( a, b, c )


toGeographic : CoordinateSystem -> Geodesy.Spheroid.Spheroid -> Geodesy.Coordinate.Coordinate -> Geodesy.Coordinate.Coordinate
toGeographic from spheroid ( a, b, c ) =
    case from of
        Geocentric ->
            geocentricToGeographic spheroid ( a, b, c )

        Geographic ->
            ( a, b, c )

        Projected p ->
            p.toGeographic spheroid ( a, b, c )


fromGeographic : CoordinateSystem -> Geodesy.Spheroid.Spheroid -> Geodesy.Coordinate.Coordinate -> Geodesy.Coordinate.Coordinate
fromGeographic to spheroid ( a, b, c ) =
    case to of
        Geocentric ->
            geographicToGeocentric spheroid ( a, b, c )

        Geographic ->
            ( a, b, c )

        Projected p ->
            p.fromGeographic spheroid ( a, b, c )


toGeocentric : CoordinateSystem -> Geodesy.Spheroid.Spheroid -> Geodesy.Coordinate.Coordinate -> Geodesy.Coordinate.Coordinate
toGeocentric from spheroid ( a, b, c ) =
    case from of
        Geocentric ->
            ( a, b, c )

        Geographic ->
            geographicToGeocentric spheroid ( a, b, c )

        Projected p ->
            p.toGeographic spheroid ( a, b, c )


fromGeocentric : CoordinateSystem -> Geodesy.Spheroid.Spheroid -> Geodesy.Coordinate.Coordinate -> Geodesy.Coordinate.Coordinate
fromGeocentric to spheroid ( a, b, c ) =
    case to of
        Geocentric ->
            ( a, b, c )

        Geographic ->
            geographicToGeocentric spheroid ( a, b, c )

        Projected p ->
            p.fromGeographic spheroid ( a, b, c )


n : Geodesy.Spheroid.Spheroid -> Float -> Float
n spheroid phi =
    spheroid.a / sqrt (1 - Geodesy.Spheroid.e2 spheroid * sin phi ^ 2)


geographicToGeocentric : Geodesy.Spheroid.Spheroid -> Geodesy.Coordinate.Coordinate -> Geodesy.Coordinate.Coordinate
geographicToGeocentric spheroid ( lon, lat, h ) =
    ( n spheroid (Geodesy.Utils.radian lat + h) * cos (Geodesy.Utils.radian lon) * cos (Geodesy.Utils.radian lat)
    , n spheroid (Geodesy.Utils.radian lat + h) * cos (Geodesy.Utils.radian lat) * sin (Geodesy.Utils.radian lon)
    , (n spheroid (Geodesy.Utils.radian lat) * (spheroid.a * (1 - Geodesy.Spheroid.f spheroid)) ^ 2 / Geodesy.Spheroid.a2 spheroid + h) * sin (Geodesy.Utils.radian lat)
    )


geocentricToGeographic : Geodesy.Spheroid.Spheroid -> Geodesy.Coordinate.Coordinate -> Geodesy.Coordinate.Coordinate
geocentricToGeographic spheroid ( x, y, z ) =
    let
        sd =
            sqrt (x ^ 2 + y ^ 2)

        tt =
            atan (z * spheroid.a / (sd * Geodesy.Spheroid.b spheroid))

        bb =
            atan ((z + Geodesy.Spheroid.e2 spheroid * Geodesy.Spheroid.a2 spheroid / Geodesy.Spheroid.b spheroid * sin tt ^ 3) / (sd - Geodesy.Spheroid.e2 spheroid * spheroid.a * cos tt ^ 3))
    in
    ( Geodesy.Utils.degree (atan2 y x)
    , Geodesy.Utils.degree bb
    , sd / cos bb - n spheroid bb
    )
