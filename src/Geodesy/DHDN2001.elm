module Geodesy.DHDN2001 exposing
    ( GK(..)
    , Geographic(..)
    , Zone(..)
    , bessel
    , geocentric
    , geographic
    , gk
    )

import Geodesy
import Geodesy.WGS84 as WGS84


type Geocentric
    = Geocentric Geodesy.Geocentric


type Geographic
    = Geographic Geodesy.Geographic


type GK
    = GK Zone Geodesy.Projected


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


bessel : Geodesy.Spheroid
bessel =
    { a = 6377397.155
    , fi = 299.1528128
    }


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
