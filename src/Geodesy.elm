module Geodesy exposing
    ( Coordinate, Spheroid, Transformation, Datum, Projection, CoordinateSystem(..), CoordinateReferenceSystem
    , convert, transform
    , wgs84, webMercator
    )

{-| A coordinate conversion and transformation library in elm. Use predefined
CoordinateSystem's, Projection's and Transformation's or create your own to convert
coordinates to any other system.


# Types

@docs Coordinate, Spheroid, Transformation, Datum, Projection, CoordinateSystem, CoordinateReferenceSystem


# Functions

@docs convert, transform


# Predefined

@docs wgs84, webMercator

-}


{-| A Coordinate is represented by three Float values.

    Geocentric: X Y Z
    Geographic: Longitude Latitude Height
    Projected East North Height

-}
import Geodesy exposing (CoordinateSystem(..))
type alias Coordinate =
    ( Float, Float, Float )


{-| A Spheroid is represented by its major axis and inverse flattening.
-}
type alias Spheroid =
    { majorAxis : Float, inverseFlattening : Float }


{-| A Projection is represented by its conversion from and to geographic
coordinate on a specific spheroid.
-}
type alias Projection =
    { toGeographic : Spheroid -> Coordinate -> Coordinate
    , fromGeographic : Spheroid -> Coordinate -> Coordinate
    }


{-| A CoordinateSystem can be Geocentric, Geographic or Projected.
-}
type CoordinateSystem
    = Geocentric
    | Geographic
    | Projected Projection


{-| A Transformation is represented by its transformation from and to
WGS84 geocentric coordinates.
-}
type alias Transformation =
    { toWGS84 : Coordinate -> Coordinate
    , fromWGS84 : Coordinate -> Coordinate
    }


{-| A (geodetic) datum is represented by a Spheroid and a Transformation.
-}
type alias Datum =
    { spheroid : Spheroid
    , transformation : Transformation
    }


{-| A CoordinateReferenceSystem is defined by its coordinate system and
geodetic datum.
-}
type alias CoordinateReferenceSystem =
    { coordinateSystem : CoordinateSystem
    , datum : Datum
    }


{-| Coordinate transformation for coordinate systems with different geodetic
datum.
-}
transform : CoordinateReferenceSystem -> CoordinateReferenceSystem -> Coordinate -> Coordinate
transform from to ( a, b, c ) =
    fromWGS84 to <| toWGS84 from ( a, b, c )


{-| Coordinate conversion for coordinate systems in the same geodetic
datum.
-}
convert : CoordinateSystem -> CoordinateSystem -> Spheroid -> Coordinate -> Coordinate
convert from to spheroid ( a, b, c ) =
    fromGeographic to spheroid <| toGeographic from spheroid ( a, b, c )


{-| The WebMercator projection is used by many web maps (like google).
-}
webMercator : Projection
webMercator =
    { toGeographic =
        \spheroid ( east, north, h ) ->
            ( degree (east / spheroid.majorAxis)
            , atan (e ^ (north / spheroid.majorAxis)) * degree 1 * 2 - 90
            , h
            )
    , fromGeographic =
        \spheroid ( lon, lat, h ) ->
            ( radian lon * spheroid.majorAxis
            , logBase e (tan (radian (90 + lat) / 2)) * spheroid.majorAxis
            , h
            )
    }


{-| WGS84 is represented by a WGS84 Spheroid. All transformations are defined
relative to WGS84.
-}
wgs84 : Datum
wgs84 =
    { spheroid = { majorAxis = 6378137, inverseFlattening = 298.257223563 }
    , transformation =
        { toWGS84 = \c -> c
        , fromWGS84 = \c -> c
        }
    }


toWGS84 : CoordinateReferenceSystem -> Coordinate -> Coordinate
toWGS84 from ( a, b, c ) =
    from.datum.transformation.toWGS84 <|
        toGeocentric from.coordinateSystem from.datum.spheroid ( a, b, c )


fromWGS84 : CoordinateReferenceSystem -> Coordinate -> Coordinate
fromWGS84 to ( x, y, z ) =
    fromGeocentric to.coordinateSystem to.datum.spheroid <|
        to.datum.transformation.fromWGS84 ( x, y, z )


toGeographic : CoordinateSystem -> Spheroid -> Coordinate -> Coordinate
toGeographic from spheroid ( a, b, c ) =
    case from of
        Geocentric ->
            geocentricToGeographic spheroid ( a, b, c )

        Geographic ->
            ( a, b, c )

        Projected p ->
            p.toGeographic spheroid ( a, b, c )


fromGeographic : CoordinateSystem -> Spheroid -> Coordinate -> Coordinate
fromGeographic to spheroid ( a, b, c ) =
    case to of
        Geocentric ->
            geographicToGeocentric spheroid ( a, b, c )

        Geographic ->
            ( a, b, c )

        Projected p ->
            p.fromGeographic spheroid ( a, b, c )


toGeocentric : CoordinateSystem -> Spheroid -> Coordinate -> Coordinate
toGeocentric from spheroid ( a, b, c ) =
    case from of
        Geocentric ->
            ( a, b, c )

        Geographic ->
            geographicToGeocentric spheroid ( a, b, c )

        Projected p ->
            p.toGeographic spheroid ( a, b, c )


fromGeocentric : CoordinateSystem -> Spheroid -> Coordinate -> Coordinate
fromGeocentric to spheroid ( a, b, c ) =
    case to of
        Geocentric ->
            ( a, b, c )

        Geographic ->
            geographicToGeocentric spheroid ( a, b, c )

        Projected p ->
            p.fromGeographic spheroid ( a, b, c )


n : Spheroid -> Float -> Float
n spheroid phi =
    spheroid.majorAxis / sqrt (1 - e2 spheroid * sin phi ^ 2)


geographicToGeocentric : Spheroid -> Coordinate -> Coordinate
geographicToGeocentric spheroid ( lon, lat, h ) =
    ( n spheroid (radian lat + h) * cos (radian lon) * cos (radian lat)
    , n spheroid (radian lat + h) * cos (radian lat) * sin (radian lon)
    , (n spheroid (radian lat) * (spheroid.majorAxis * (1 - f spheroid)) ^ 2 / a2 spheroid + h) * sin (radian lat)
    )


geocentricToGeographic : Spheroid -> Coordinate -> Coordinate
geocentricToGeographic spheroid ( x, y, z ) =
    let
        sd =
            sqrt (x ^ 2 + y ^ 2)

        tt =
            atan (z * spheroid.majorAxis / (sd * minorFlattening spheroid))

        bb =
            atan ((z + e2 spheroid * a2 spheroid / minorFlattening spheroid * sin tt ^ 3) / (sd - e2 spheroid * spheroid.majorAxis * cos tt ^ 3))
    in
    ( degree (atan2 y x)
    , degree bb
    , sd / cos bb - n spheroid bb
    )


a2 : Spheroid -> Float
a2 spheroid =
    spheroid.majorAxis ^ 2


f : Spheroid -> Float
f spheroid =
    1 / spheroid.inverseFlattening


f2 : Spheroid -> Float
f2 spheroid =
    f spheroid ^ 2


minorFlattening : Spheroid -> Float
minorFlattening spheroid =
    spheroid.majorAxis * (1 - f spheroid)


e2 : Spheroid -> Float
e2 spheroid =
    2 / spheroid.inverseFlattening - f2 spheroid


e_ : Spheroid -> Float
e_ spheroid =
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


degree : Float -> Float
degree rad =
    rad * 180 / pi


radian : Float -> Float
radian deg =
    deg * pi / 180
