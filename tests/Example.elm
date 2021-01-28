module Example exposing (..)

import Expect exposing (Expectation)
import Geodesy
import Geodesy.ETRS89 as ETRS89
import Geodesy.WGS84 as WGS84
import Round exposing (roundNum)
import Test exposing (..)


transverseMercator : Test
transverseMercator =
    let
        ( toGeographic, _ ) =
            Geodesy.transverseMercator
                { falseNorthing = 0
                , falseEasting = 500000
                , scale = 0.9996
                , longitudeOfOrigin = 32 * 6 - 183
                , latitudeOfOrigin = 0
                }

        r =
            toGeographic WGS84.spheroid { east = 568649.7, north = 5761510.32 }
    in
    test "test TransverseMercator"
        (\_ ->
            Expect.equal
                { lon = 9.999999971769741, lat = 51.999999729925996 }
                { lon = r.lon, lat = r.lat }
        )


webMercator : Test
webMercator =
    test "test webMercator"
        (\_ ->
            Expect.equal
                { east = 1113194.9079327355, north = 6800125.454397307, h = 0 }
                (Geodesy.convert Geodesy.geographic (Geodesy.projected Geodesy.webMercator) WGS84.spheroid { lon = 10, lat = 52, h = 0 })
        )


wgs84ToETRS89UTM : Test
wgs84ToETRS89UTM =
    test "test wgs84ToETRS89UTM"
        (\_ ->
            Expect.equal
                (ETRS89.UTM ETRS89.Zone32 { east = 568649.7015227115, north = 5761510.45470585, h = 0.00006500817835330963 })
                (WGS84.transform WGS84.geographic ETRS89.utm (WGS84.Geographic { lon = 10, lat = 52, h = 0 }))
        )
