module Geodesy exposing
    ( CoordinateSystem
    , Geocentric
    , Geographic
    , Geographic2
    , Projected
    , Projected2
    , Projection
    , Spheroid
    , convert
    , geocentric
    , geographic
    , normalizeLongitude
    , projected
    , transverseMercator
    , webMercator
    )


type alias Spheroid =
    { a : Float, fi : Float }


type alias Geocentric =
    { x : Float, y : Float, z : Float }


type alias Geographic =
    { lon : Float, lat : Float, h : Float }


type alias Projected =
    { east : Float, north : Float, h : Float }


type alias CoordinateSystem x =
    ( Spheroid -> x -> Geocentric, Spheroid -> Geocentric -> x )


type alias Geographic2 =
    { lon : Float, lat : Float }


type alias Projected2 =
    { east : Float, north : Float }


type alias Projection =
    ( Spheroid -> Projected2 -> Geographic2, Spheroid -> Geographic2 -> Projected2 )


normalizeLongitude : Float -> Float
normalizeLongitude lon =
    if lon < 180 && lon >= -180 then
        lon

    else if lon < 180 then
        normalizeLongitude (lon + 360)

    else
        normalizeLongitude (lon - 360)


convert : CoordinateSystem x -> CoordinateSystem y -> Spheroid -> x -> y
convert ( toGeocentric, _ ) ( _, fromGeocentric ) s x =
    fromGeocentric s (toGeocentric s x)


geographic : CoordinateSystem Geographic
geographic =
    let
        n_ : Spheroid -> Float -> Float
        n_ s phi =
            s.a / sqrt (1 - e2 s * sin phi ^ 2)
    in
    ( \s c ->
        { x = n_ s (radian c.lat + c.h) * cos (radian c.lon) * cos (radian c.lat)
        , y = n_ s (radian c.lat + c.h) * cos (radian c.lat) * sin (radian c.lon)
        , z = (n_ s (radian c.lat) * (s.a * (1 - f s)) ^ 2 / a2 s + c.h) * sin (radian c.lat)
        }
    , \s c ->
        let
            sd_ =
                sqrt (c.x ^ 2 + c.y ^ 2)

            t_ =
                atan (c.z * s.a / (sd_ * b s))

            b_ =
                atan ((c.z + e2 s * a2 s / b s * sin t_ ^ 3) / (sd_ - e2 s * s.a * cos t_ ^ 3))
        in
        { lon = degree (atan2 c.y c.x)
        , lat = degree b_
        , h = sd_ / cos b_ - n_ s b_
        }
    )


geocentric : CoordinateSystem Geocentric
geocentric =
    ( \_ c -> c, \_ c -> c )


projected : Projection -> CoordinateSystem Projected
projected ( toGeographic, fromGeographic ) =
    let
        ( toGeocentric, fromGeocentric ) =
            geographic
    in
    ( \s p ->
        let
            gg =
                toGeographic s { east = p.east, north = p.north }
        in
        toGeocentric s { lon = gg.lon, lat = gg.lat, h = p.h }
    , \s g ->
        let
            gg =
                fromGeocentric s g

            p =
                fromGeographic s { lon = gg.lon, lat = gg.lat }
        in
        { east = p.east, north = p.north, h = gg.h }
    )


webMercator : Projection
webMercator =
    ( \s c ->
        { lon = degree (c.east / s.a)
        , lat = atan (e ^ (c.north / s.a)) * degree 1 * 2 - 90
        }
    , \s c ->
        { east = radian c.lon * s.a
        , north = logBase e (tan (radian (90 + c.lat) / 2)) * s.a
        }
    )


type alias TransverseMercator =
    { longitudeOfOrigin : Float
    , latitudeOfOrigin : Float
    , scale : Float
    , falseEasting : Float
    , falseNorthing : Float
    }


transverseMercator : TransverseMercator -> Projection
transverseMercator { longitudeOfOrigin, latitudeOfOrigin, scale, falseEasting, falseNorthing } =
    let
        m_ : Spheroid -> Float -> Float
        m_ s phi =
            s.a
                * ((1 - e2 s / 4 - 3 * e4 s / 64 - 5 * e6 s / 256)
                    * phi
                    - (3 * e2 s / 8 + 3 * e4 s / 32 + 45 * e6 s / 1024)
                    * sin (2 * phi)
                    + (15 * e4 s / 256 + 45 * e6 s / 1024)
                    * sin (4 * phi)
                    - (35 * e6 s / 3072)
                    * sin (6 * phi)
                  )

        n_ : Spheroid -> Float -> Float
        n_ s phi =
            s.a / sqrt (1 - e2 s * sin phi ^ 2)

        t_ : Float -> Float
        t_ phi =
            tan phi ^ 2

        c_ : Spheroid -> Float -> Float
        c_ s phi =
            ei2 s * cos phi ^ 2
    in
    ( \s { east, north } ->
        let
            east_ =
                east - falseEasting

            north_ =
                north - falseNorthing

            mi_ =
                m_ s (radian latitudeOfOrigin) + north_ / scale

            my =
                mi_ / (s.a * (1 - e2 s / 4 - 3 * e4 s / 64 - 5 * e6 s / 256))

            phi1_ =
                my
                    + (3 * ei s / 2 - 27 * ei3 s / 32)
                    * sin (2 * my)
                    + (21 * ei2 s / 16 - 55 * ei4 s / 32)
                    * sin (4 * my)
                    + (151 * ei3 s / 96)
                    * sin (6 * my)
                    + (1097 * ei4 s / 512)
                    * sin (8 * my)

            r1_ =
                s.a * (1 - e2 s) / (1 - e2 s * sin phi1_ ^ 2) ^ (3 / 2)

            d_ =
                east_ / (n_ s phi1_ * scale)

            phi_ =
                phi1_
                    - (n_ s phi1_ * tan phi1_ / r1_)
                    * (d_
                        * d_
                        / 2
                        - (5
                            + 3
                            * t_ phi1_
                            + 10
                            * c_ s phi1_
                            - 4
                            * c_ s phi1_
                            * c_ s phi1_
                            - 9
                            * ei2 s
                          )
                        * d_
                        ^ 4
                        / 24
                        + (61
                            + 90
                            * t_ phi1_
                            + 298
                            * c_ s phi1_
                            + 45
                            * t_ phi1_
                            * t_ phi1_
                            - 252
                            * ei2 s
                            - 3
                            * c_ s phi1_
                            * c_ s phi1_
                          )
                        * d_
                        ^ 6
                        / 720
                      )

            lambda =
                radian longitudeOfOrigin
                    + (d_
                        - (1 + 2 * t_ phi1_ + c_ s phi1_)
                        * d_
                        * d_
                        * d_
                        / 6
                        + (5
                            - 2
                            * c_ s phi1_
                            + 28
                            * t_ phi1_
                            - 3
                            * c_ s phi1_
                            * c_ s phi1_
                            + 8
                            * ei2 s
                            + 24
                            * t_ phi1_
                            * t_ phi1_
                          )
                        * (d_
                            ^ 5
                          )
                        / 120
                      )
                    / cos phi1_
        in
        { lon = degree lambda
        , lat = degree phi_
        }
    , \s { lon, lat } ->
        let
            phi_ =
                radian lat

            a_ =
                (radian lon - radian longitudeOfOrigin) * cos phi_
        in
        { east =
            scale
                * n_ s phi_
                * (a_
                    + (1 - t_ phi_ + c_ s phi_)
                    * (a_ ^ 3)
                    / 6
                    + (5
                        - 18
                        * t_ phi_
                        + t_ phi_
                        * t_ phi_
                        + 72
                        * c_ s phi_
                        - 58
                        * ei2 s
                      )
                    * (a_ ^ 5)
                    / 120
                  )
                + falseEasting
        , north =
            scale
                * (m_ s phi_
                    - m_ s (radian latitudeOfOrigin)
                    + n_ s phi_
                    * tan phi_
                    * (a_
                        * a_
                        / 2
                        + (5
                            - t_ phi_
                            + 9
                            * c_ s phi_
                            + 4
                            * c_ s phi_
                            * c_ s phi_
                          )
                        * (a_ ^ 4)
                        / 24
                        + (61
                            - 58
                            * t_ phi_
                            + t_ phi_
                            * t_ phi_
                            + 600
                            * c_ s phi_
                            - 330
                            * ei2 s
                          )
                        * (a_ ^ 6)
                        / 720
                      )
                  )
                + falseNorthing
        }
    )


degree : Float -> Float
degree rad =
    rad * 180 / pi


radian : Float -> Float
radian deg =
    deg * pi / 180


a2 : Spheroid -> Float
a2 s =
    s.a ^ 2


f : Spheroid -> Float
f s =
    1 / s.fi


f2 : Spheroid -> Float
f2 s =
    f s ^ 2


b : Spheroid -> Float
b s =
    s.a * (1 - f s)


e2 : Spheroid -> Float
e2 s =
    2 / s.fi - f2 s


e4 : Spheroid -> Float
e4 s =
    e2 s ^ 2


e6 : Spheroid -> Float
e6 s =
    e2 s ^ 4


ei : Spheroid -> Float
ei s =
    (1 - sqrt (1 - e2 s)) / (1 + sqrt (1 - e2 s))


ei2 : Spheroid -> Float
ei2 s =
    ei s ^ 2


ei3 : Spheroid -> Float
ei3 s =
    ei s ^ 3


ei4 : Spheroid -> Float
ei4 s =
    ei s ^ 4
