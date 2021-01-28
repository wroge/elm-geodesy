module Geodesy.ETRS89 exposing
    ( Geographic(..)
    , UTM(..)
    , Zone(..)
    , geocentric
    , geographic
    , grs80
    , toZone
    , utm
    )

import Geodesy
import Geodesy.WGS84 as WGS84


type Geographic
    = Geographic Geodesy.Geographic


type UTM
    = UTM Zone Geodesy.Projected


type Zone
    = Zone28
    | Zone29
    | Zone30
    | Zone31
    | Zone32
    | Zone33
    | Zone34
    | Zone35
    | Zone36
    | Zone37
    | Zone38


toZone : Float -> Zone
toZone lon =
    let
        zone =
            round ((Geodesy.normalizeLongitude lon + 183) / 6)
    in
    if zone < 28 then
        Zone28

    else
        case zone of
            28 ->
                Zone28

            29 ->
                Zone29

            30 ->
                Zone30

            31 ->
                Zone31

            32 ->
                Zone32

            33 ->
                Zone33

            34 ->
                Zone34

            35 ->
                Zone35

            36 ->
                Zone36

            37 ->
                Zone37

            _ ->
                Zone38


getZone : Zone -> Float
getZone z =
    case z of
        Zone28 ->
            28

        Zone29 ->
            29

        Zone30 ->
            30

        Zone31 ->
            31

        Zone32 ->
            32

        Zone33 ->
            33

        Zone34 ->
            34

        Zone35 ->
            35

        Zone36 ->
            36

        Zone37 ->
            37

        Zone38 ->
            38


grs80 : Geodesy.Spheroid
grs80 =
    { a = 6378137, fi = 298.257222101 }


geocentric : WGS84.CoordinateReferenceSystem WGS84.Geocentric
geocentric =
    WGS84.geocentric


geographic : WGS84.CoordinateReferenceSystem Geographic
geographic =
    let
        ( toGeocentric, fromGeocentric ) =
            Geodesy.geographic
    in
    ( \c ->
        case c of
            Geographic gg ->
                WGS84.Geocentric (toGeocentric grs80 gg)
    , \c ->
        case c of
            WGS84.Geocentric gc ->
                Geographic (fromGeocentric grs80 gc)
    )


utm : WGS84.CoordinateReferenceSystem UTM
utm =
    let
        ( toWGS84, fromWGS84 ) =
            geographic

        projection : Zone -> Geodesy.Projection
        projection zone =
            Geodesy.transverseMercator
                { falseNorthing = 0
                , falseEasting = 500000
                , scale = 0.9996
                , longitudeOfOrigin = getZone zone * 6 - 183
                , latitudeOfOrigin = 0
                }
    in
    ( \c ->
        case c of
            UTM z p ->
                let
                    ( toGeographic, _ ) =
                        projection z

                    p2 =
                        toGeographic grs80 { east = p.east, north = p.north }
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
                        fromGeographic grs80 { lon = gg.lon, lat = gg.lat }
                in
                UTM z { east = pp.east, north = pp.north, h = gg.h }
    )
