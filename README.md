
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DRI.GLUCoSE

<!-- badges: start -->

<!-- badges: end -->

The goal of DRI.GLUCoSE is to provide a public package containing
functions and code used in the development of the DRI-GLUCoSE Index.

## Installation

You can install the latest version of DRI.GLUCoSE from
[GitHub](https://CRAN.R-project.org) with:

``` r
devtools::install_git("https://github.com/STBrinkmann/DRI.GLUCoSE")
```

## Usage

Once installed, the library can be loaded as follows:

``` r
library(DRI.GLUCoSE)
#> Loading required package: dplyr
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
#> Loading required package: sf
#> Linking to GEOS 3.8.0, GDAL 3.0.4, PROJ 6.3.1
```

### Isochrone workflow

One key purpose of this package is, to provide functions for route
networked derived isochrones. For that purpose we have provided a simple
sf object of 4 points in Erlangen, Germany.

``` r
data(Erlangen)
Erlangen
#> Simple feature collection with 4 features and 2 fields
#> geometry type:  POINT
#> dimension:      XY
#> bbox:           xmin: 35199.46 ymin: -159433.5 xmax: 36631.72 ymax: -158452.3
#> projected CRS:  ETRS89 / LCC Germany (N-E)
#> # A tibble: 4 x 3
#>      id Speed                 geom
#>   <int> <dbl>          <POINT [m]>
#> 1     1  78.5 (35199.46 -159433.5)
#> 2     2  79.8 (36281.59 -159243.2)
#> 3     3  76.5 (35676.69 -158452.3)
#> 4     4  79.8 (36631.72 -158460.3)
```

The first step is to download and preprocess OSM road data:

``` r
erlangen.osm <- osm_roads(x = Erlangen, dist = 20, speed = "Speed", cores = 4)
```

Next, the isodistances will be calculated based on the road network.
Since the road network contains a lot of features (n=22337), this will
take some time (\~30-60 minutes).

``` r
erlangen.isodistances <- isodistances(x = Erlangen, road_network = erlangen.osm, tag = "id",
                                      isochrones_seq = seq(2, 20, 2), speed = "Speed", cores = 4)
```

Finally, the isochrones can be calculated by applying an off-road buffer
on the isodistances. The results will be mapped using the
[tmap](https://github.com/mtennekes/tmap) package.

``` r
erlangen.isochrones <- isochrones(erlangen.isodistances, 40, 4)
```
