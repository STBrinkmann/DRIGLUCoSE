
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DRI-GLUCoSE

<!-- badges: start -->

<!-- badges: end -->

The goal of DRIGLUCoSE is to provide a public package containing
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

## Isochrone workflow

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
on the isodistances. The results can be mapped using the
[tmap](https://github.com/mtennekes/tmap) package, however at this point
I do not know how to include the .html map into the GitHub-ReadMe.

``` r
erlangen.isochrones <- isochrones(erlangen.isodistances, 40, 4)
```

``` r
tmap::tmap_mode("view")

tmap1 <- tmap::tm_shape(Erlangen[Erlangen$id == 1,] %>% sf::st_buffer(20*Erlangen[Erlangen$id == 1,]$Speed),
               name = "Euclidean Buffer") +
  tmap::tm_polygons(alpha = 0.1, legend.show = F) +
  tmap::tm_shape(erlangen.isochrones[erlangen.isochrones$tag == 1, ] %>% dplyr::mutate(time = forcats::fct_rev(time))) +
  tmap::tm_polygons(col="time",
                    palette = "Dark2",
                    alpha = 0.1,
                    legend.show = FALSE) +
  tmap::tm_facets(by = "time", as.layers = TRUE, free.coords = FALSE, drop.units = TRUE) +
  tmap::tm_shape(erlangen.isodistances[erlangen.isodistances$tag == 1, ] %>% dplyr::mutate(time = forcats::fct_rev(time))) +
  tmap::tm_lines(col="time",
                 palette = "Dark2",
                 lwd = 3,
                 legend.col.show = FALSE,
                 legend.lwd.show = FALSE) +
  tmap::tm_facets(by = "time", as.layers = TRUE, free.coords = FALSE, drop.units = TRUE) +
  tmap::tm_shape(Erlangen[Erlangen$id == 1,], name = "Centre") +
  tmap::tm_dots(col = "black", size = 0.1) +
  tmap::tm_layout(legend.outside = TRUE) +
  tmap::tm_basemap(leaflet::providers$OpenStreetMap)

tmap1
```
