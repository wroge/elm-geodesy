# Coordinate Conversion Library for elm!

### Install

```
elm install wroge/elm-geodesy
```

### Usage

```elm
import Geodesy exposing (convert, Geographic, Projected, webMercator, wgs84)

lonlath = convert Geographic (Projected webMercator) wgs84.spheroid ( 10, 52, 0 )
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
