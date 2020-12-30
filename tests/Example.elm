module Example exposing (..)

import Expect exposing (Expectation)
import Geodesy
import Geodesy.Projection
import Geodesy.Spheroid
import Test exposing (..)


geographicToWebMercator : Test
geographicToWebMercator =
    test "geographicToWebMercator"
        (\_ ->
            Expect.equal
                ( 1113194.9079327357, 6800125.454397307, 0 )
                (Geodesy.convert Geodesy.Geographic (Geodesy.Projected Geodesy.Projection.webMercator) Geodesy.Spheroid.wgs84 ( 10, 52, 0 ))
        )


webMercatorToGeographic : Test
webMercatorToGeographic =
    test "webMercatorToGeographic"
        (\_ ->
            Expect.equal
                ( 10, 52, 0 )
                (Geodesy.convert (Geodesy.Projected Geodesy.Projection.webMercator) Geodesy.Geographic Geodesy.Spheroid.wgs84 ( 1113194.9079327357, 6800125.454397307, 0 ))
        )


geographicToGeocentric : Test
geographicToGeocentric =
    test "geographicToGeocentric"
        (\_ ->
            Expect.equal
                ( 3875179.5753784818, 683298.714229599, 5002803.345482635 )
                (Geodesy.convert Geodesy.Geographic Geodesy.Geocentric Geodesy.Spheroid.wgs84 ( 10, 52, 0 ))
        )


geocentricToGeographic : Test
geocentricToGeographic =
    test "geocentricToGeographic"
        (\_ ->
            Expect.equal
                ( 9.999999999999998, 52.00000000000001, 0 )
                (Geodesy.convert Geodesy.Geocentric Geodesy.Geographic Geodesy.Spheroid.wgs84 ( 3875179.5753784818, 683298.714229599, 5002803.345482635 ))
        )
