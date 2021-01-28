module Geodesy.WGS84 exposing
    ( CoordinateReferenceSystem
    , Geocentric(..)
    , Geographic(..)
    , UTM(..)
    , WebMercator(..)
    , Zone(..)
    , geocentric
    , geographic
    , helmert
    , spheroid
    , transform
    , utm
    , webMercator
    )

import Geodesy


type alias CoordinateReferenceSystem x =
    ( x -> Geocentric, Geocentric -> x )


type Geocentric
    = Geocentric Geodesy.Geocentric


type Geographic
    = Geographic Geodesy.Geographic


type WebMercator
    = WebMercator Geodesy.Projected


type UTM
    = UTM Zone Geodesy.Projected


type Zone
    = Zone01
    | Zone02
    | Zone03
    | Zone04
    | Zone05
    | Zone06
    | Zone07
    | Zone08
    | Zone09
    | Zone10
    | Zone11
    | Zone12
    | Zone13
    | Zone14
    | Zone15
    | Zone16
    | Zone17
    | Zone18
    | Zone19
    | Zone20
    | Zone21
    | Zone22
    | Zone23
    | Zone24
    | Zone25
    | Zone26
    | Zone27
    | Zone28
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
    | Zone39
    | Zone40
    | Zone41
    | Zone42
    | Zone43
    | Zone44
    | Zone45
    | Zone46
    | Zone47
    | Zone48
    | Zone49
    | Zone50
    | Zone51
    | Zone52
    | Zone53
    | Zone54
    | Zone55
    | Zone56
    | Zone57
    | Zone58
    | Zone59
    | Zone60


toZone : Float -> Zone
toZone lon =
    let
        zone =
            round ((Geodesy.normalizeLongitude lon + 183) / 6)
    in
    if zone < 0 then
        Zone01

    else
        case zone of
            1 ->
                Zone01

            2 ->
                Zone02

            3 ->
                Zone03

            4 ->
                Zone04

            5 ->
                Zone05

            6 ->
                Zone06

            7 ->
                Zone07

            8 ->
                Zone08

            9 ->
                Zone09

            10 ->
                Zone10

            11 ->
                Zone11

            12 ->
                Zone12

            13 ->
                Zone13

            14 ->
                Zone14

            15 ->
                Zone15

            16 ->
                Zone16

            17 ->
                Zone17

            18 ->
                Zone18

            19 ->
                Zone19

            20 ->
                Zone20

            21 ->
                Zone21

            22 ->
                Zone22

            23 ->
                Zone23

            24 ->
                Zone24

            25 ->
                Zone25

            26 ->
                Zone26

            27 ->
                Zone27

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

            38 ->
                Zone38

            39 ->
                Zone39

            40 ->
                Zone40

            41 ->
                Zone41

            42 ->
                Zone42

            43 ->
                Zone43

            44 ->
                Zone44

            45 ->
                Zone45

            46 ->
                Zone46

            47 ->
                Zone47

            48 ->
                Zone48

            49 ->
                Zone49

            50 ->
                Zone50

            51 ->
                Zone51

            52 ->
                Zone52

            53 ->
                Zone53

            54 ->
                Zone54

            55 ->
                Zone55

            56 ->
                Zone56

            57 ->
                Zone57

            58 ->
                Zone58

            59 ->
                Zone59

            _ ->
                Zone60


getZone : Zone -> Float
getZone z =
    case z of
        Zone01 ->
            1

        Zone02 ->
            2

        Zone03 ->
            3

        Zone04 ->
            4

        Zone05 ->
            5

        Zone06 ->
            6

        Zone07 ->
            7

        Zone08 ->
            8

        Zone09 ->
            9

        Zone10 ->
            10

        Zone11 ->
            11

        Zone12 ->
            12

        Zone13 ->
            13

        Zone14 ->
            14

        Zone15 ->
            15

        Zone16 ->
            16

        Zone17 ->
            17

        Zone18 ->
            18

        Zone19 ->
            19

        Zone20 ->
            20

        Zone21 ->
            21

        Zone22 ->
            22

        Zone23 ->
            23

        Zone24 ->
            24

        Zone25 ->
            25

        Zone26 ->
            26

        Zone27 ->
            27

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

        Zone39 ->
            39

        Zone40 ->
            40

        Zone41 ->
            41

        Zone42 ->
            42

        Zone43 ->
            43

        Zone44 ->
            44

        Zone45 ->
            45

        Zone46 ->
            46

        Zone47 ->
            47

        Zone48 ->
            48

        Zone49 ->
            49

        Zone50 ->
            50

        Zone51 ->
            51

        Zone52 ->
            52

        Zone53 ->
            53

        Zone54 ->
            54

        Zone55 ->
            55

        Zone56 ->
            56

        Zone57 ->
            57

        Zone58 ->
            58

        Zone59 ->
            59

        Zone60 ->
            60


spheroid : Geodesy.Spheroid
spheroid =
    { a = 6378137, fi = 298.257223563 }


geocentric : CoordinateReferenceSystem Geocentric
geocentric =
    ( \c -> c, \c -> c )


geographic : CoordinateReferenceSystem Geographic
geographic =
    let
        ( toGeocentric, fromGeocentric ) =
            Geodesy.geographic
    in
    ( \c ->
        case c of
            Geographic gg ->
                Geocentric (toGeocentric spheroid gg)
    , \c ->
        case c of
            Geocentric gc ->
                Geographic (fromGeocentric spheroid gc)
    )


webMercator : CoordinateReferenceSystem WebMercator
webMercator =
    let
        ( toGeocentric, fromGeocentric ) =
            Geodesy.projected Geodesy.webMercator
    in
    ( \c ->
        case c of
            WebMercator gg ->
                Geocentric (toGeocentric spheroid gg)
    , \c ->
        case c of
            Geocentric gc ->
                WebMercator (fromGeocentric spheroid gc)
    )


utm : CoordinateReferenceSystem UTM
utm =
    let
        tm zone =
            { falseNorthing = 0
            , falseEasting = 500000
            , scale = 0.9996
            , longitudeOfOrigin = getZone zone * 6 - 183
            , latitudeOfOrigin = 0
            }

        ( toGeocentric, fromGeocentric ) =
            Geodesy.geographic

        projection : Zone -> Geodesy.Projection
        projection zone =
            Geodesy.transverseMercator
                (tm zone)
    in
    ( \c ->
        case c of
            UTM z p ->
                let
                    ( toGeographic, _ ) =
                        projection z

                    gg =
                        toGeographic spheroid { east = p.east, north = p.north }
                in
                Geocentric (toGeocentric spheroid { lon = gg.lon, lat = gg.lat, h = p.h })
    , \c ->
        case c of
            Geocentric gc ->
                let
                    gg =
                        fromGeocentric spheroid gc

                    z =
                        toZone gg.lon

                    ( _, fromGeographic ) =
                        projection z

                    pp =
                        fromGeographic spheroid { lon = gg.lon, lat = gg.lat }
                in
                UTM z { east = pp.east, north = pp.north, h = gg.h }
    )


transform : CoordinateReferenceSystem x -> CoordinateReferenceSystem y -> x -> y
transform ( toWGS84, _ ) ( _, fromWGS84 ) x =
    fromWGS84 (toWGS84 x)


type alias Helmert =
    { tx : Float
    , ty : Float
    , tz : Float
    , rx : Float
    , ry : Float
    , rz : Float
    , ds : Float
    }


helmert : Helmert -> CoordinateReferenceSystem Geodesy.Geocentric
helmert h =
    ( forward h, inverse h )


calcHelmert : Helmert -> Geodesy.Geocentric -> Geodesy.Geocentric
calcHelmert { tx, ty, tz, rx, ry, rz, ds } c =
    { x = (1 + ds * ppm) * (c.x + c.z * ry * asec - c.y * rz * asec) + tx
    , y = (1 + ds * ppm) * (c.y + c.x * rz * asec - c.z * rx * asec) + ty
    , z = (1 + ds * ppm) * (c.z + c.y * rx * asec - c.x * ry * asec) + tz
    }


forward : Helmert -> Geodesy.Geocentric -> Geocentric
forward h c =
    Geocentric (calcHelmert h c)


inverse : Helmert -> Geocentric -> Geodesy.Geocentric
inverse { tx, ty, tz, rx, ry, rz, ds } c =
    case c of
        Geocentric gc ->
            calcHelmert { tx = -tx, ty = -ty, tz = -tz, rx = -rx, ry = -ry, rz = -rz, ds = -ds } gc


asec : Float
asec =
    pi / 648000


ppm : Float
ppm =
    0.000001
