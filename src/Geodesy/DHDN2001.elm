module Geodesy.DHDN2001 exposing
    ( Geocentric(..), Geographic(..), GK(..), Zone(..)
    , bessel, geocentric, geographic, gk
    )

{-| A coordinate transformation library for dhdn2001 coordinate reference systems in elm.


# Types

@docs Geocentric, Geographic, GK, Zone


# Functions

@docs bessel, geocentric, geographic, gk

-}

import Geodesy
import Geodesy.WGS84 as WGS84


{-| Geocentric is represented by x, y, and z coordinates.
-}
type Geocentric
    = Geocentric Geodesy.Geocentric


{-| Geographic is represented by longitude, latitude and height coordinates.
-}
type Geographic
    = Geographic Geodesy.Geographic


{-| GK is represented by a zone and northern, eastern and height coordinates.
-}
type GK
    = GK Zone Geodesy.Projected


{-| Zone represents the three DHDN2001 Zones 3, 4 and 5.
-}
type Zone
    = Zone3
    | Zone4
    | Zone5


toZone : Float -> Zone
toZone lon =
    let
        zone =
            round (Geodesy.normalizeLongitude lon / 3)
    in
    if zone < 3 then
        Zone3

    else
        case zone of
            3 ->
                Zone3

            4 ->
                Zone4

            _ ->
                Zone5


getZone : Zone -> Float
getZone z =
    case z of
        Zone3 ->
            3

        Zone4 ->
            4

        Zone5 ->
            5


{-| bessel represents Bessel Spheroid.
-}
bessel : Geodesy.Spheroid
bessel =
    { a = 6377397.155
    , fi = 299.1528128
    }


{-| geocentric represents the DHDN2001 geocentric coordinate reference system.
-}
geocentric : WGS84.CoordinateReferenceSystem Geocentric
geocentric =
    let
        ( toWGS84, fromWGS84 ) =
            WGS84.helmert
                { tx = 598.1
                , ty = 73.7
                , tz = 418.2
                , rx = 0.202
                , ry = 0.045
                , rz = -2.455
                , ds = 6.7
                }
    in
    ( \c ->
        case c of
            Geocentric gg ->
                toWGS84 gg
    , \c ->
        Geocentric (fromWGS84 c)
    )


{-| geographic represents the DHDN2001 geographic coordinate reference system.
-}
geographic : WGS84.CoordinateReferenceSystem Geographic
geographic =
    let
        ( toWGS84, fromWGS84 ) =
            geocentric

        ( toGeocentric, fromGeocentric ) =
            Geodesy.geographic
    in
    ( \c ->
        case c of
            Geographic gg ->
                toWGS84 (Geocentric (toGeocentric bessel gg))
    , \c ->
        case fromWGS84 c of
            Geocentric gc ->
                Geographic (fromGeocentric bessel gc)
    )


{-| gk represents the DHDN2001 gauss kruger coordinate reference system.
-}
gk : WGS84.CoordinateReferenceSystem GK
gk =
    let
        ( toWGS84, fromWGS84 ) =
            geographic

        projection : Zone -> Geodesy.Projection
        projection zone =
            Geodesy.transverseMercator
                { falseNorthing = 0
                , falseEasting = getZone zone * 1000000 + 500000
                , scale = 1
                , longitudeOfOrigin = getZone zone * 3
                , latitudeOfOrigin = 0
                }
    in
    ( \c ->
        case c of
            GK z p ->
                let
                    ( toGeographic, _ ) =
                        projection z

                    p2 =
                        toGeographic bessel { east = p.east, north = p.north }
                in
                toWGS84 (Geographic { lon = p2.lon, lat = p2.lat, h = p.h })
    , \c ->
        case fromWGS84 c of
            Geographic gg ->
                let
                    z =
                        toZone gg.lon

                    ( _, fromGeographic ) =
                        projection z

                    pp =
                        fromGeographic bessel { lon = gg.lon, lat = gg.lat }
                in
                GK z { east = pp.east, north = pp.north, h = gg.h }
    )
