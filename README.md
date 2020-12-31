# Coordinate Conversion Library for elm!

### Install

```
elm install wroge/elm-geodesy
```

### Usage

```elm
import Geodesy


eastNorthHeight : ( Float, Float, Float )
eastNorthHeight =
    Geodesy.convert Geodesy.Geographic (Geodesy.Projected Geodesy.webMercator) Geodesy.wgs84.spheroid ( 10, 52, 0 )
```

### Including

- [x] Geocentric
- [x] Geographic
- [x] WebMercator
- [x] WGS84
- [ ] Helmert
- [ ] GRS80
- [ ] ETRS80
- [ ] Lambert Conformal Conic
- [ ] Albers Equal Area Conic
- [ ] Transverse Mercator (UTM)
- [ ] EPSG-Codes
