
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DRI-GLUCoSE

<!-- badges: start -->

<!-- badges: end -->

As elaborated in our recent analyses (Walker et al. 2019; Scarpone et
al. 2020), nearly all previous studies in the literature use either
census unit boundaries or simple buffer zones to measure an individual’s
built environment (BE) exposures or to characterize their local
socioeconomic status (SES) (Rhew et al. 2011, Gong et al. 2014, Fuertes
et al. 2014). Therefore, we present a distance-weighted, network-based
model for quantifying the combined effects of local greenspace and SES
on diabetes risk, from which we derive an area-based Diabetes Risk Index
of Greenspace, Land Use and Socioeconomic Environments (DRI-GLUCoSE).  
The goal of the DRIGLUCoSE package is to provide a public package
containing functions and code used in the development of the DRI-GLUCoSE
Index.

## Installation

You can install the latest version of DRIGLUCoSE from
[GitHub](https://CRAN.R-project.org) with:

``` r
devtools::install_git("https://github.com/STBrinkmann/DRIGLUCoSE")
```

## Usage

Once installed, the library can be loaded as follows:

``` r
library(DRIGLUCoSE)
```

# Methods

One key purpose of this package is, to provide functions for route
networked derived isochrones. For that purpose we have provided a simple
sf object of 2 points in Erlangen, Germany.

``` r
data(Erlangen)
Erlangen
#> Simple feature collection with 2 features and 2 fields
#> geometry type:  POINT
#> dimension:      XY
#> bbox:           xmin: 35199.46 ymin: -159433.5 xmax: 36281.59 ymax: -159243.2
#> projected CRS:  ETRS89 / LCC Germany (N-E)
#> # A tibble: 2 x 3
#>      id Speed                 geom
#>   <dbl> <dbl>          <POINT [m]>
#> 1     1  78.5 (35199.46 -159433.5)
#> 2     2  79.8 (36281.59 -159243.2)
```

## Census variables

In our analysis we acquired data of the Canadian census dissemination
areas (DA, the smallest available census area with an average population
of 400-800 residents) for the census year 2006 from the federal
government agency Statistics Canada. It has been converted to a
shapefile (sf) with one column per census variable. To demonstrate we
use the following randomly generated data:

``` r
set.seed(1234)
census <- sf::st_make_grid(
  # Use Sample Data and apply 25 minutes buffer (Speed[m/min] * 25[min]) 
  Erlangen %>% dplyr::mutate(geom = sf::st_buffer(geom, Speed*25)),
  cellsize = 100
  ) %>% 
  st_as_sf() %>% 
  mutate(census_var = sample(1000:10000, n(), replace = TRUE)) %>% 
  rename(geom = x)

census
#> Simple feature collection with 2142 features and 1 field
#> geometry type:  POLYGON
#> dimension:      XY
#> bbox:           xmin: 33236.96 ymin: -161396 xmax: 38336.96 ymax: -157196
#> projected CRS:  ETRS89 / LCC Germany (N-E)
#> First 10 features:
#>    census_var                           geom
#> 1        8451 POLYGON ((33236.96 -161396,...
#> 2        9015 POLYGON ((33336.96 -161396,...
#> 3        8161 POLYGON ((33436.96 -161396,...
#> 4        9085 POLYGON ((33536.96 -161396,...
#> 5        8268 POLYGON ((33636.96 -161396,...
#> 6        1622 POLYGON ((33736.96 -161396,...
#> 7        1933 POLYGON ((33836.96 -161396,...
#> 8        3947 POLYGON ((33936.96 -161396,...
#> 9        3145 POLYGON ((34036.96 -161396,...
#> 10       3773 POLYGON ((34136.96 -161396,...
```

## Greenspace

In our analysis we acquired LANDSAT images through the United States
Geological Survey’s EarthExplorer platform
(\[<https://earthexplorer.usgs.gov/>\]). The Normalized Difference
Vegetation Index (NDVI) is used as a metric to model greenspace
exposure. Pre-processing of the LANDSAT images and NDVI calculation has
been conducted using the *LS\_L1C* function:

``` r
DRIGLUCoSE::LS_L1C(l1c_path = "docs/LC08_L1TP_193026_20200423_20200508_01_T1_small/", 
                   out_dir = "docs/LS_PreProcessed",
                   # Use Sample Data and apply 25 minutes buffer (Speed[m/min] * 25[min]) 
                   sf_mask = DRIGLUCoSE::Erlangen %>% 
                     dplyr::mutate(geom = sf::st_buffer(geom, Speed*25)),
                   cores = 20)
```

## Exposure Model

In order to estimate each participant’s potential exposures to
greenspace and local SES, we (i) mapped age- and sex-specific walkable
zones around their residential address, and (ii) applied a negative
logit weighting function, such that the estimated effect of greenspace
or SES decreases as distance from the home increases.

### (i) Road network data and isochrones

In order to compute network-based distance metrics, we acquired street
data from OpenStreetMap using the R-package osmdata (Padgham et
al. 2017). Road types not suitable for walking were removed (e.g.,
motorways). Network data were topologically corrected and split into
\~30 metre-long segments using the R package *nngeo* (Dorman 2020).

``` r
erlangen.osm <- DRIGLUCoSE::osm_roads(x = Erlangen, dist = 20, 
                                      speed = "Speed", cores = 2)
erlangen.osm
```

This network data was used to derive walking distance buffers for each
participant, based on age- and sex-specific walking speed estimates
derived from the literature (Dewulf 2012). Starting from each
participant’s place of residence, we computed network-constrained
buffers with an off-road width of 40 meters, running in 2-minute
increments from 0 to 20 minutes, using the A\*-algorithm (Hart, Nilsson
& Raphael 1968). This therefore resulted in each participant having ten
concentric isochrones, the sizes of which are a function of participant
age and sex.  
Since the road network contains a lot of features (n=14292), this will
take some time (\~15-30 minutes).

``` r
erlangen.isodistances <- DRIGLUCoSE::isodistances(x = Erlangen, 
                                                  road_network = erlangen.osm, 
                                                  tag = "id", speed = "Speed",
                                                  isochrones_seq = seq(2, 20, 2),
                                                  cores = 2)

erlangen.isochrones <- DRIGLUCoSE::isochrones(x = erlangen.isodistances, 
                                              buffer = 40, cores = 2)
```

``` r
erlangen.isodistances %>%
  dplyr::mutate(time = forcats::fct_rev(time)) %>% 
  dplyr::filter(id == 2) %>% 
  ggplot2::ggplot(aes(colour = time)) + 
  ggplot2::geom_sf(size = 1) + 
  ggplot2::scale_color_discrete(name = "Walking-Time\n(minutes)") +
  ggthemes::theme_map() + 
  ggplot2::theme(legend.position = c(0.96, 0.35),
                 legend.title=element_text(size=18), 
                 legend.text=element_text(size=14))
```

## Distance-weighting

In order to account for the diminishing effect of SES and greenspace
exposure as distance increases, we fitted a logit function to weight
each incremental isochrone, such that the influence of a variable
decreases with increasing distance from the household, i.e., features
that are farther away have less influence than nearby features, as
illustrated in Figure 2. A logit function was selected as it
heuristically approximates a suitable distance-decay function (Bauer and
Groneberg 2016; Jia et al. 2019) and various parameterisations (for
parameters *b* and *m*) of the logit function were assessed using
sensitivity analysis.  
The distance-weighting is separated in two parts, first the logit
function (1) that is used for both SES and greenspace variables, and
second the proportional weights function (4) that is only applied on SES
variables.

  
![&#10;\\begin{align}&#10; G\_t =&#10; \\begin{cases}&#10;
\\cfrac{\\int\_0^{r\_t} g(r)dr}{\\int\_0^{t\_max} g(r)dr}, t=1\\\\&#10;
\\cfrac{\\int\_{t-1}^{r\_t} g(r)dr}{\\int\_0^{t\_max} g(r)dr}, t\>1&#10;
\\end{cases}&#10;\\end{align}&#10;](https://latex.codecogs.com/png.latex?%0A%5Cbegin%7Balign%7D%0A%20%20G_t%20%3D%0A%20%20%20%20%5Cbegin%7Bcases%7D%0A%20%20%20%20%20%20%5Ccfrac%7B%5Cint_0%5E%7Br_t%7D%20g%28r%29dr%7D%7B%5Cint_0%5E%7Bt_max%7D%20g%28r%29dr%7D%2C%20t%3D1%5C%5C%0A%20%20%20%20%20%20%5Ccfrac%7B%5Cint_%7Bt-1%7D%5E%7Br_t%7D%20g%28r%29dr%7D%7B%5Cint_0%5E%7Bt_max%7D%20g%28r%29dr%7D%2C%20t%3E1%0A%20%20%20%20%5Cend%7Bcases%7D%0A%5Cend%7Balign%7D%0A
"
\\begin{align}
  G_t =
    \\begin{cases}
      \\cfrac{\\int_0^{r_t} g(r)dr}{\\int_0^{t_max} g(r)dr}, t=1\\\\
      \\cfrac{\\int_{t-1}^{r_t} g(r)dr}{\\int_0^{t_max} g(r)dr}, t\>1
    \\end{cases}
\\end{align}
")
