DRI-GLUCoSE
================

-   [Installation](#installation)
-   [Methods](#methods)
    -   [Census variables](#census-variables)
    -   [Greenspace](#greenspace)
    -   [Exposure Model](#exposure-model)
        -   [(i) Road network data and
            isochrones](#i-road-network-data-and-isochrones)
        -   [(ii) Distance-weighting](#ii-distance-weighting)
-   [Appendix](#appendix)
    -   [Figures](#figures)
    -   [Tables](#tables)
    -   [Summary Statistics](#summary-statistics)
-   [About](#about)
    -   [Package contributors](#package-contributors)
    -   [Thesis authors](#thesis-authors)
-   [Bibliography](#bibliography)

[![DOI:10.1007/s11524-022-00630-w](https://zenodo.org/badge/DOI/10.1007/s11524-022-00630-w.svg)](https://doi.org/10.1007/s11524-022-00630-w)

As elaborated in our recent analyses (Walker et al. 2019; Scarpone et
al. 2020), nearly all previous studies in the literature use either
census unit boundaries or simple buffer zones to measure an individual’s
built environment (BE) exposures or to characterize their local
socioeconomic status (SES) (Gong et al. 2014; Fuertes et al. 2014).
Therefore, we present a distance-weighted, network-based model for
quantifying the combined effects of local greenspace and SES on diabetes
risk, from which we derive an area-based Diabetes Risk Index of
Greenspace, Land Use and Socioeconomic Environments (DRI-GLUCoSE). The
goal of the `DRIGLUCoSE` package is to provide a public package
containing functions and code used in the development of the DRI-GLUCoSE
Index(Walker et al. 2022).

# Installation

You can install the latest version of `DRIGLUCoSE` from GitHub with:

``` r
remotes::install_git("https://github.com/STBrinkmann/DRIGLUCoSE")
```

Once installed, the library can be loaded as follows:

``` r
library(DRIGLUCoSE)
```

# Methods

One key purpose of this package is, to provide functions for route
networked derived isochrones. For that purpose we have provided a sample
sf object of 2 points in Erlangen, Germany.

``` r
data(Erlangen)
Erlangen
## Simple feature collection with 2 features and 2 fields
## Geometry type: POINT
## Dimension:     XY
## Bounding box:  xmin: 35199.46 ymin: -159433.5 xmax: 36281.59 ymax: -159243.2
## Projected CRS: ETRS89 / LCC Germany (N-E)
## # A tibble: 2 x 3
##     tag Speed                 geom
##   <dbl> <dbl>          <POINT [m]>
## 1     1  78.5 (35199.46 -159433.5)
## 2     2  79.8 (36281.59 -159243.2)
```

## Census variables

In our analysis we acquired data of the Canadian census dissemination
areas. It has been converted to a shapefile (sf) with one column per
census variable. To demonstrate we use the following randomly generated
data:

``` r
set.seed(1234)
census <- sf::st_make_grid(
  # Use Sample Data and apply 25 minutes buffer (Speed[m/min] * 25[min]) 
  Erlangen %>% dplyr::mutate(geom = sf::st_buffer(geom, Speed*25)),
  cellsize = 100
  ) %>% 
  sf::st_as_sf() %>% 
  dplyr::mutate(census_var_a = sample(1:1000, n(), replace = TRUE),
                census_var_b = sample(1000:10000, n(), replace = TRUE),
                census_var_c = sample(100000:150000, n(), replace = TRUE)) %>% 
  dplyr::rename(geom = x)

census
## Simple feature collection with 2142 features and 3 fields
## Geometry type: POLYGON
## Dimension:     XY
## Bounding box:  xmin: 33236.96 ymin: -161396 xmax: 38336.96 ymax: -157196
## Projected CRS: ETRS89 / LCC Germany (N-E)
## First 10 features:
##    census_var_a census_var_b census_var_c                           geom
## 1           284         2009       114811 POLYGON ((33236.96 -161396,...
## 2           848         3318       139961 POLYGON ((33336.96 -161396,...
## 3           918         3954       112673 POLYGON ((33436.96 -161396,...
## 4           101         3359       137505 POLYGON ((33536.96 -161396,...
## 5           623         3107       109201 POLYGON ((33636.96 -161396,...
## 6           905         2630       131656 POLYGON ((33736.96 -161396,...
## 7           645         6512       135789 POLYGON ((33836.96 -161396,...
## 8           934         9945       115006 POLYGON ((33936.96 -161396,...
## 9           400         3583       111379 POLYGON ((34036.96 -161396,...
## 10          900         7778       137576 POLYGON ((34136.96 -161396,...
```

## Greenspace

In our analysis we acquired LANDSAT images through the United States
Geological Survey’s EarthExplorer platform
(<https://earthexplorer.usgs.gov/>). The Normalized Difference
Vegetation Index
([NDVI](https://gisgeography.com/ndvi-normalized-difference-vegetation-index/))
is used as a metric to model greenspace exposure. Pre-processing of the
LANDSAT images and NDVI calculation has been conducted using the
`LS_L1C` function:

``` r
DRIGLUCoSE::LS_L1C(l1c_path = "docs/LC08_L1TP_193026_20200423_20200508_01_T1_small/", 
                   out_dir = "docs/LS_PreProcessed",
                   # Use Sample Data and apply 25 minutes buffer (Speed[m/min] * 25[min]) 
                   sf_mask = DRIGLUCoSE::Erlangen %>% 
                     dplyr::mutate(geom = sf::st_buffer(geom, Speed*25)),
                   cores = 20)
## Project raster
## DN to TOA Reflectance
## class      : RasterStack 
## dimensions : 122, 151, 18422, 8  (nrow, ncol, ncell, nlayers)
## resolution : 30, 30  (x, y)
## extent     : 33493.69, 38023.69, -161164.2, -157504.2  (xmin, xmax, ymin, ymax)
## crs        : +proj=lcc +lat_0=51 +lon_0=10.5 +lat_1=48.6666666666667 +lat_2=53.6666666666667 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs 
## names      :      Blue,     Green,       Red,       NIR,     SWIR1,     SWIR2,      NDWI,      NDVI 
## min values :         0,         0,         0,         0,         0,         0,        -1,        -1 
## max values : 0.2020575, 0.2322532, 0.3076383, 0.5424371, 0.4233773, 0.3753066, 1.0000000, 1.0000000
```

## Exposure Model

In order to estimate each participant’s potential exposures to
greenspace and local SES, we (i) mapped age- and sex-specific walkable
zones around their residential address, and (ii) applied a negative
logit weighting function, such that the estimated effect of greenspace
or SES decreases as distance from the home increases.

### (i) Road network data and isochrones

In order to compute network-based distance metrics, we acquired street
data from OpenStreetMap using the R-package `osmdata` (Padgham et al.
2017). Road types not suitable for walking were removed (e.g.,
motorways). Network data were topologically corrected and split into
\~20 metre-long segments using the R package `nngeo` (Michael Dorman
2020).

``` r
erlangen.osm <- DRIGLUCoSE::osm_roads(x = Erlangen, dist = 20, 
                                      speed = "Speed", cores = 2)
```

This network data was used to derive walking distance buffers for each
participant, based on walking speed. Starting from each participant’s
place of residence, we computed network-constrained buffers with an
off-road width of 40 meters, running in 2-minute increments from 0 to 20
minutes, using the A\*-algorithm (Hart, Nilsson, and Raphael 1968). This
therefore resulted in each participant having ten concentric isochrones,
the sizes of which are a function of individual walking speed and road
network.

``` r
erlangen.isodistances <- DRIGLUCoSE::isodistances(x = Erlangen, 
                                                  road_network = erlangen.osm, 
                                                  tag = "tag", speed = "Speed",
                                                  isochrones_seq = seq(2, 20, 2),
                                                  cores = 2)
```

``` r
erlangen.isochrones <- DRIGLUCoSE::isochrones(x = erlangen.isodistances, 
                                              buffer = 40, cores = 2)
erlangen.isochrones
## Simple feature collection with 20 features and 2 fields
## Geometry type: MULTIPOLYGON
## Dimension:     XY
## Bounding box:  xmin: 34034 ymin: -160951 xmax: 37750 ymax: -157782
## Projected CRS: ETRS89 / LCC Germany (N-E)
## # A tibble: 20 x 3
##     time   tag                                                              geom
##  * <dbl> <dbl>                                                <MULTIPOLYGON [m]>
##  1     2     1 (((35292.18 -159582.1, 35292.75 -159584.1, 35293.22 -159586.1, 3~
##  2     4     1 (((35314.38 -159725.3, 35313.74 -159727.3, 35312.99 -159729.2, 3~
##  3     6     1 (((35348.37 -159830.5, 35347.9 -159831.2, 35347.44 -159832, 3534~
##  4     8     1 (((35282 -159969.8, 35281.96 -159971.9, 35281.8 -159974, 35281.5~
##  5    10     1 (((35249.78 -160125.6, 35249.57 -160125.5, 35248.86 -160125.1, 3~
##  6    12     1 (((35264.36 -160300.8, 35263.71 -160300.8, 35263.06 -160300.9, 3~
##  7    14     1 (((35379.71 -160416.5, 35380.79 -160418.3, 35381.78 -160420.1, 3~
##  8    16     1 (((35412 -160571, 35411.95 -160573.1, 35411.78 -160575.2, 35411.~
##  9    18     1 (((35430.23 -160426.8, 35430.28 -160426.3, 35430.28 -160426.3, 3~
## 10    20     1 (((35546 -160803.2, 35546.09 -160803.5, 35546.22 -160804.2, 3554~
## 11     2     2 (((36385.71 -159372.9, 36384.21 -159374.3, 36382.64 -159375.7, 3~
## 12     4     2 (((36379.78 -159545.8, 36379.83 -159546.5, 36379.9 -159547.1, 36~
## 13     6     2 (((36368.01 -159688.3, 36366.95 -159690.1, 36365.8 -159691.9, 36~
## 14     8     2 (((36655.01 -159679.5, 36655.61 -159681.6, 36656.11 -159683.6, 3~
## 15    10     2 (((36520.27 -159907.5, 36518.33 -159908.3, 36516.36 -159909, 365~
## 16    12     2 (((36053.07 -160058.5, 36051.72 -160056.9, 36050.46 -160055.3, 3~
## 17    14     2 (((36832.82 -159976.8, 36832.8 -159977, 36832.79 -159977.1, 3683~
## 18    16     2 (((36016.71 -160325, 36016.5 -160325.4, 36016.23 -160325.9, 3601~
## 19    18     2 (((36020.67 -160327.7, 36022.64 -160327, 36024.65 -160326.4, 360~
## 20    20     2 (((36689.36 -160321.8, 36690.36 -160320, 36691.45 -160318.2, 366~
```

Figure 1 shows isodistances of the two points of the sample data in
Erlangen, Germany.

<img src="man/figures/README-unnamed-chunk-10-1.svg" width="100%" />

### (ii) Distance-weighting

In order to account for the diminishing effect of SES and greenspace
exposure as distance increases, we fitted a logit function to weight
each incremental isochrone, such that the influence of a variable
decreases with increasing distance from the household, i.e., features
that are farther away have less influence than nearby features, as
illustrated in Figure 2. A logit function was selected as it
heuristically approximates a suitable distance-decay function (Bauer and
Groneberg 2016; Jia, Wang, and Xierali 2019). The distance-weighting is
separated in two parts, first the logit function (1) that is used for
both SES and greenspace variables, and second the proportional weights
function (4) that is only applied on SES variables.

<center>

![
\\begin{align\*}
  G_t =
    \\begin{cases}
      \\cfrac{\\int_0^{r_t} \\, g(r)dr}{\\int_0^{r\_{t\_{max}}} \\, g(r)dr}, t=1\\\\
      \\cfrac{\\int\_{r\_{t-1}}^{r_t}  \\, g(r)dr}{\\int_0^{r\_{t\_{max}}} \\, g(r)dr}, t\>1
    \\end{cases}
    && \\text{(1)}
\\end{align\*}
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0A%5Cbegin%7Balign%2A%7D%0A%20%20G_t%20%3D%0A%20%20%20%20%5Cbegin%7Bcases%7D%0A%20%20%20%20%20%20%5Ccfrac%7B%5Cint_0%5E%7Br_t%7D%20%5C%2C%20g%28r%29dr%7D%7B%5Cint_0%5E%7Br_%7Bt_%7Bmax%7D%7D%7D%20%5C%2C%20g%28r%29dr%7D%2C%20t%3D1%5C%5C%0A%20%20%20%20%20%20%5Ccfrac%7B%5Cint_%7Br_%7Bt-1%7D%7D%5E%7Br_t%7D%20%20%5C%2C%20g%28r%29dr%7D%7B%5Cint_0%5E%7Br_%7Bt_%7Bmax%7D%7D%7D%20%5C%2C%20g%28r%29dr%7D%2C%20t%3E1%0A%20%20%20%20%5Cend%7Bcases%7D%0A%20%20%20%20%26%26%20%5Ctext%7B%281%29%7D%0A%5Cend%7Balign%2A%7D%0A "
\begin{align*}
  G_t =
    \begin{cases}
      \cfrac{\int_0^{r_t} \, g(r)dr}{\int_0^{r_{t_{max}}} \, g(r)dr}, t=1\\
      \cfrac{\int_{r_{t-1}}^{r_t}  \, g(r)dr}{\int_0^{r_{t_{max}}} \, g(r)dr}, t>1
    \end{cases}
    && \text{(1)}
\end{align*}
")

</center>

Each isochrone
![t](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;t "t")
is assigned a distance weight
![G_T](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;G_T "G_T"),
calculated as the integral of the logistic distance decay function
![g(r)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;g%28r%29 "g(r)")
(2)

<center>

![
\\begin{align\*}
  g(r) =
    \\cfrac{1}{1 + e^{ \\,b \\,(r-m)}}
    && \\text{(2)}
\\end{align\*}
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0A%5Cbegin%7Balign%2A%7D%0A%20%20g%28r%29%20%3D%0A%20%20%20%20%5Ccfrac%7B1%7D%7B1%20%2B%20e%5E%7B%20%5C%2Cb%20%5C%2C%28r-m%29%7D%7D%0A%20%20%20%20%26%26%20%5Ctext%7B%282%29%7D%0A%5Cend%7Balign%2A%7D%0A "
\begin{align*}
  g(r) =
    \cfrac{1}{1 + e^{ \,b \,(r-m)}}
    && \text{(2)}
\end{align*}
")

</center>

with
![b = 8](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;b%20%3D%208 "b = 8")
and
![m = 0.6](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;m%20%3D%200.6 "m = 0.6"),
in the interval between the mean inner radius
![r\_{t-1}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;r_%7Bt-1%7D "r_{t-1}")
and mean outer radius
![r_t](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;r_t "r_t")
of the isochrone (e.g. 2 to 4 minutes isochrones), normalized by the
integral from 0 to the outermost isochrone boundary
![r\_{t\_{max}}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;r_%7Bt_%7Bmax%7D%7D "r_{t_{max}}")
(e.g. 20 minutes isochrone). Weighted summary statistics to describe the
greenspace (e.g. mean or minimum NDVI) are thus described as (3)

<center>

![
\\begin{align\*}
  \\sum_t G_t \\, f(NDVI_t \\, \\cap \\, I_t)
    && \\text{(3)}
\\end{align\*}
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0A%5Cbegin%7Balign%2A%7D%0A%20%20%5Csum_t%20G_t%20%5C%2C%20f%28NDVI_t%20%5C%2C%20%5Ccap%20%5C%2C%20I_t%29%0A%20%20%20%20%26%26%20%5Ctext%7B%283%29%7D%0A%5Cend%7Balign%2A%7D%0A "
\begin{align*}
  \sum_t G_t \, f(NDVI_t \, \cap \, I_t)
    && \text{(3)}
\end{align*}
")

</center>

For SES variables the proportional weights of the census areas within
the isochrone are further defined as (4)

<center>

![
\\begin{align\*}
  A\_{tj} =
    \\cfrac{A(C_j \\, \\cap \\, I_t)}
    {A(I_t)}
    && \\text{(4)}
\\end{align\*}
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0A%5Cbegin%7Balign%2A%7D%0A%20%20A_%7Btj%7D%20%3D%0A%20%20%20%20%5Ccfrac%7BA%28C_j%20%5C%2C%20%5Ccap%20%5C%2C%20I_t%29%7D%0A%20%20%20%20%7BA%28I_t%29%7D%0A%20%20%20%20%26%26%20%5Ctext%7B%284%29%7D%0A%5Cend%7Balign%2A%7D%0A "
\begin{align*}
  A_{tj} =
    \cfrac{A(C_j \, \cap \, I_t)}
    {A(I_t)}
    && \text{(4)}
\end{align*}
")

</center>

with the proportion of the area of the intersection of the census area
![C_j](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;C_j "C_j")
and the isochrone
![I_t](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;I_t "I_t"),
and the area of the isochrone
![I_t](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;I_t "I_t").
The weighted value of the SES variable
![x_i](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;x_i "x_i")
in the census area
![j](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;j "j")
is then defined as (5)

<center>

![
\\begin{align\*}
  \\sum_t \\left( \\ G_t \\ \\sum_j \\, x\_{ij} \\; A{tj} \\right)
    && \\text{(5)}
\\end{align\*}
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0A%5Cbegin%7Balign%2A%7D%0A%20%20%5Csum_t%20%5Cleft%28%20%5C%20G_t%20%5C%20%5Csum_j%20%5C%2C%20x_%7Bij%7D%20%5C%3B%20A%7Btj%7D%20%5Cright%29%0A%20%20%20%20%26%26%20%5Ctext%7B%285%29%7D%0A%5Cend%7Balign%2A%7D%0A "
\begin{align*}
  \sum_t \left( \ G_t \ \sum_j \, x_{ij} \; A{tj} \right)
    && \text{(5)}
\end{align*}
")

</center>

Figure 2 visualizes the different submodels used for distance-weighting
SES and greenspace. Fig. 2a shows the unweighted values of a SES
variable and fig. 2b has been calculated using (5), thus representing
the proportional weights of all intersections with the census areas and
isochrones. Greenspace is weighted as shown in fig. 2c using (3).

<img src="docs/WeightsPlot.svg" title="Figure 2: Unweighted values (a) and network-based distance-weighting function for socioeconomic variables (b) and greenspace (c). Bold black lines indicate the isochrones." alt="Figure 2: Unweighted values (a) and network-based distance-weighting function for socioeconomic variables (b) and greenspace (c). Bold black lines indicate the isochrones." width="90%" />

The distance-weighting for the LANDSAT derived NDVI raster (greenspace
exposure) is handled using `LS_band_weightin`, and SES distance- and
areal-weighting using `census_weighting`.

``` r
# Calculate sd, median, 5th percentile, 95th percentile and skew of NDVI values
NDVI_weighted <- 
  DRIGLUCoSE::LS_band_weighting(isochrones = erlangen.isochrones, tag = "tag",
                                landsat_list = dir("docs/LS_PreProcessed",
                                                   pattern = ".grd",
                                                   full.names = T) %>%
                                  lapply(raster::brick),
                                stats = list("sd", "median", 
                                             list("percentile", 0.05), 
                                             list("percentile", 0.95),
                                             "skew"), 
                                b = 8, m = 0.6, cores = 2)

NDVI_weighted
## # A tibble: 2 x 6
##     tag    sd median X5_percentile X95_percentile    skew
##   <dbl> <dbl>  <dbl>         <dbl>          <dbl>   <dbl>
## 1     1 0.205  0.615         0.281          0.914 -0.145 
## 2     2 0.104  0.540         0.360          0.714 -0.0122
```

``` r
census_weighted <- DRIGLUCoSE::census_weighting(isochrones = erlangen.isochrones, 
                                                tag = "tag", census = census, 
                                                b = 8, m = 0.6, cores = 2)
census_weighted
## # A tibble: 2 x 4
##     tag census_var_a census_var_b census_var_c
##   <dbl>        <dbl>        <dbl>        <dbl>
## 1     1         562.        5323.      130970.
## 2     2         547.        5419.      124610.
```

# Appendix

## Figures

DRI-GLUCoSE scores for Vancouver (top) and Hamilton (bottom), ranging
from low risk (purple) to high risk areas (orange).

<img src="docs/DRI-GLUCoSE Index - Vancouver.png" width="90%" />
<img src="docs/DRI-GLUCoSE Index - Hamilton.png" width="90%" />



<img src="docs/forest_plot_big.svg" title="Figure A.1: Forest plot showing significant effects for both BMI- and WHR-controlled multivariable logistic models." alt="Figure A.1: Forest plot showing significant effects for both BMI- and WHR-controlled multivariable logistic models." width="90%" />

<img src="docs/roc_auc.svg" title="Figure A.2: The ROC curves for both BMI- and WHR-controlled multivariable logistic models." alt="Figure A.2: The ROC curves for both BMI- and WHR-controlled multivariable logistic models." width="90%" />

## Tables

<table style="width:90%; font-family: &quot;Arial Narrow&quot;, &quot;Source Sans Pro&quot;, sans-serif; width: auto !important; margin-left: auto; margin-right: auto;" class=" lightable-classic lightable-striped">
<caption>
Table A.1: Logistic models for all multivariable models with odds ratios
(OR) for diabetes, 95% CI, and p-values.
</caption>
<thead>
<tr>
<th style="empty-cells: hide;" colspan="2">
</th>
<th style="padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; font-style: italic; " colspan="2">

<div style="TRUE">

Semi-Adjusted

</div>

</th>
<th style="padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; font-style: italic; " colspan="2">

<div style="TRUE">

Fully-Adjusted

</div>

</th>
</tr>
<tr>
<th style="text-align:left;font-weight: bold;">
Parameter
</th>
<th style="text-align:right;font-weight: bold;">
OR<br>(bivariate)
</th>
<th style="text-align:right;font-weight: bold;">
OR<br>(WHR-adjusted)
</th>
<th style="text-align:right;font-weight: bold;">
OR<br>(BMI-adjusted)
</th>
<th style="text-align:right;font-weight: bold;">
OR<br>(WHR-adjusted)
</th>
<th style="text-align:right;font-weight: bold;">
OR<br>(BMI-adjusted)
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
DRI-GLUCoSE Score
</td>
<td style="text-align:right;">
0.37 (0.26-0.51,<br>p&lt0.001)
</td>
<td style="text-align:right;">
0.41 (0.31-0.53,<br>p&lt0.001)
</td>
<td style="text-align:right;">
0.46 (0.35-0.61,<br>p&lt0.001)
</td>
<td style="text-align:right;">
0.45 (0.36-0.56,<br>p&lt0.001)
</td>
<td style="text-align:right;">
0.50 (0.37-0.67,<br>p&lt0.001)
</td>
</tr>
<tr>
<td style="text-align:left;">
Age (5 year-interval)
</td>
<td style="text-align:right;">
1.26 (1.18-1.34,<br>p&lt0.001)
</td>
<td style="text-align:right;">
1.21 (1.15-1.27,<br>p&lt0.001)
</td>
<td style="text-align:right;">
1.26 (1.20-1.33,<br>p&lt0.001)
</td>
<td style="text-align:right;">
1.20 (1.15-1.65,<br>p&lt0.001)
</td>
<td style="text-align:right;">
1.24 (1.18-1.31,<br>p&lt0.001)
</td>
</tr>
<tr>
<td style="text-align:left;">
Sex: female
</td>
<td style="text-align:right;">
0.63 (0.50-0.78,<br>p&lt0.001)
</td>
<td style="text-align:right;">
0.85 (0.71-1.00,<br>p=0.056)
</td>
<td style="text-align:right;">
0.47 (0.39-0.57,<br>p&lt0.001)
</td>
<td style="text-align:right;">
0.95 (0.81-1.11,<br>p=0.498)
</td>
<td style="text-align:right;">
0.59 (0.48-0.71,<br>p&lt0.001)
</td>
</tr>
<tr>
<td style="text-align:left;">
Obese (WHR)
</td>
<td style="text-align:right;">
5.72 (4.37-7.59,<br>p&lt0.001)
</td>
<td style="text-align:right;">
5.39 (4.50-6.46,<br>p&lt0.001)
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
5.06 (4.29-5.97,<br>p&lt0.001)
</td>
<td style="text-align:right;">
</td>
</tr>
<tr>
<td style="text-align:left;">
BMI
</td>
<td style="text-align:right;">
1.14 (1.12-1.16,<br>p&lt0.001)
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
1.14 (1.12-1.15,<br>p&lt0.001)
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
1.13 (1.11-1.15,<br>p&lt0.001)
</td>
</tr>
<tr>
<td style="text-align:left;">
Household income range
</td>
<td style="text-align:right;">
0.75 (0.70-0.81,<br>p&lt0.001)
</td>
<td style="text-align:right;">
0.87 (0.82-0.92,<br>p&lt0.001)
</td>
<td style="text-align:right;">
0.85 (0.80-0.90,<br>p&lt0.001)
</td>
<td style="text-align:right;">
0.90 (0.86-0.95,<br>p&lt0.001)
</td>
<td style="text-align:right;">
0.85 (0.80-0.91,<br>p&lt0.001)
</td>
</tr>
<tr>
<td style="text-align:left;">
Neighbourhood type: urban
</td>
<td style="text-align:right;">
0.93 (0.71-1.21,<br>p=0.605)
</td>
<td style="text-align:right;">
0.58 (0.47-0.72,<br>p&lt0.001)
</td>
<td style="text-align:right;">
0.62 (0.50-0.77,<br>p&lt0.001)
</td>
<td style="text-align:right;">
0.60 (0.50-0.72,<br>p&lt0.001)
</td>
<td style="text-align:right;">
0.66 (0.52-0.84,<br>p&lt0.001)
</td>
</tr>
<tr>
<td style="text-align:left;">
AHEI Score (E^1)
</td>
<td style="text-align:right;">
0.78 (0.70-0.88,<br>p&lt0.001)
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
0.95 (0.88-1.03,<br>p=0.197)
</td>
<td style="text-align:right;">
0.99 (0.89-1.09,<br>p=0.771)
</td>
</tr>
<tr>
<td style="text-align:left;">
Recreation Met Score: &gt=525
</td>
<td style="text-align:right;">
0.58 (0.46-0.73,<br>p&lt0.001)
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
0.82 (0.71-1.03,<br>p=0.006)
</td>
<td style="text-align:right;">
0.85 (0.71-1.03,<br>p=0.090)
</td>
</tr>
<tr>
<td style="text-align:left;">
Current/Former smoker: yes
</td>
<td style="text-align:right;">
1.65 (1.31-2.07,<br>p&lt0.001)
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
1.55 (1.34-1.80,<br>p&lt0.001)
</td>
<td style="text-align:right;">
1.38 (1.15-1.66,<br>p&lt0.001)
</td>
</tr>
<tr>
<td style="text-align:left;">
Alcohol: &lt1 drink/day
</td>
<td style="text-align:right;">
1.17 (0.91-1.51,<br>p=0.217)
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
1.81 (1.54-2.14,<br>p&lt0.001)
</td>
<td style="text-align:right;">
1.34 (1.09-1.65,<br>p=0.006)
</td>
</tr>
</tbody>
</table>

To analyse the effect of socioeconomic status (SES) and greenspace (GS),
we further build multivariable models using the semi-adjusted model with
BMI as obesity measurent and tested different combinations for the index
variable.

<table style="width:90%;border-bottom: 0; font-family: &quot;Arial Narrow&quot;, &quot;Source Sans Pro&quot;, sans-serif; width: auto !important; margin-left: auto; margin-right: auto;" class=" lightable-classic">
<caption>
Table A.2: Model Performance and odds ratios of the logistic models,
comparing combinations of socioeconomic status (SES) and greenspace (GS)
as index.
</caption>
<thead>
<tr>
<th style="text-align:left;font-weight: bold;">
Metric
</th>
<th style="text-align:right;font-weight: bold;">
SES + GS
</th>
<th style="text-align:right;font-weight: bold;">
SES
</th>
<th style="text-align:right;font-weight: bold;">
GS
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Probability Threshold *
</td>
<td style="text-align:right;">
0.47
</td>
<td style="text-align:right;">
0.50
</td>
<td style="text-align:right;">
0.48
</td>
</tr>
<tr>
<td style="text-align:left;">
Accuracy
</td>
<td style="text-align:right;">
0.75
</td>
<td style="text-align:right;">
0.72
</td>
<td style="text-align:right;">
0.74
</td>
</tr>
<tr>
<td style="text-align:left;">
Sensitivity
</td>
<td style="text-align:right;">
0.76
</td>
<td style="text-align:right;">
0.72
</td>
<td style="text-align:right;">
0.75
</td>
</tr>
<tr>
<td style="text-align:left;">
Specificity
</td>
<td style="text-align:right;">
0.65
</td>
<td style="text-align:right;">
0.68
</td>
<td style="text-align:right;">
0.68
</td>
</tr>
<tr>
<td style="text-align:left;">
Youden index
</td>
<td style="text-align:right;">
0.41
</td>
<td style="text-align:right;">
0.40
</td>
<td style="text-align:right;">
0.43
</td>
</tr>
<tr>
<td style="text-align:left;">
OR (95% CI, p-value)
</td>
<td style="text-align:right;">
0.46 (0.35-0.61, p < 0.001)
</td>
<td style="text-align:right;">
0.57 (0.42-0.76, p < 0.001)
</td>
<td style="text-align:right;">
0.42 (0.31-0.57, p < 0.001)
</td>
</tr>
</tbody>
<tfoot>
<tr>
<td style="padding: 0; " colspan="100%">
<sup>*</sup> Probability threshold used for predicting Diabetes. Values
equal or greater than this threshold are mapped as “No”.
</td>
</tr>
</tfoot>
</table>
<table style="width:90%;border-bottom: 0; font-family: &quot;Arial Narrow&quot;, &quot;Source Sans Pro&quot;, sans-serif; width: auto !important; margin-left: auto; margin-right: auto;" class=" lightable-classic">
<caption>
Table A.3: Model Performance for all multivariable models.
</caption>
<thead>
<tr>
<th style="empty-cells: hide;" colspan="1">
</th>
<th style="padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; font-style: italic; " colspan="2">

<div style="border-bottom: 1px solid #111111; margin-bottom: -1px; ">

Semi-Adjusted

</div>

</th>
<th style="padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; font-style: italic; " colspan="2">

<div style="border-bottom: 1px solid #111111; margin-bottom: -1px; ">

Fully-Adjusted

</div>

</th>
</tr>
<tr>
<th style="text-align:left;font-weight: bold;">
Metric
</th>
<th style="text-align:center;font-weight: bold;">
OR<br>(WHR-adjusted)
</th>
<th style="text-align:center;font-weight: bold;">
OR<br>(BMI-adjusted)
</th>
<th style="text-align:center;font-weight: bold;">
OR<br>(WHR-adjusted)
</th>
<th style="text-align:center;font-weight: bold;">
OR<br>(BMI-adjusted)
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Probability Threshold *
</td>
<td style="text-align:center;">
0.44
</td>
<td style="text-align:center;">
0.47
</td>
<td style="text-align:center;">
0.58
</td>
<td style="text-align:center;">
0.60
</td>
</tr>
<tr>
<td style="text-align:left;">
Accuracy
</td>
<td style="text-align:center;">
0.71
</td>
<td style="text-align:center;">
0.75
</td>
<td style="text-align:center;">
0.64
</td>
<td style="text-align:center;">
0.64
</td>
</tr>
<tr>
<td style="text-align:left;">
Sensitivity
</td>
<td style="text-align:center;">
0.72
</td>
<td style="text-align:center;">
0.76
</td>
<td style="text-align:center;">
0.62
</td>
<td style="text-align:center;">
0.62
</td>
</tr>
<tr>
<td style="text-align:left;">
Specificity
</td>
<td style="text-align:center;">
0.70
</td>
<td style="text-align:center;">
0.65
</td>
<td style="text-align:center;">
0.81
</td>
<td style="text-align:center;">
0.83
</td>
</tr>
<tr>
<td style="text-align:left;">
Youden index
</td>
<td style="text-align:center;">
0.42
</td>
<td style="text-align:center;">
0.41
</td>
<td style="text-align:center;">
0.43
</td>
<td style="text-align:center;">
0.45
</td>
</tr>
</tbody>
<tfoot>
<tr>
<td style="padding: 0; " colspan="100%">
<sup>*</sup> Probability threshold used for predicting Diabetes. Values
equal or greater than this threshold are mapped as “No”.
</td>
</tr>
</tfoot>
</table>

## Summary Statistics

<table style="width:90%; font-family: &quot;Arial Narrow&quot;, &quot;Source Sans Pro&quot;, sans-serif; width: auto !important; margin-left: auto; margin-right: auto;" class=" lightable-classic">
<caption>
Table A.4: Baseline characteristics of the study population by diabetes
status.
</caption>
<thead>
<tr>
<th style="text-align:left;font-weight: bold;">
Parameter
</th>
<th style="text-align:left;font-weight: bold;">
No Diabetes (N=4616)
</th>
<th style="text-align:left;font-weight: bold;">
Diabetes (N=509)
</th>
<th style="text-align:left;font-weight: bold;">
Total (N=5125)
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Total included
</td>
<td style="text-align:left;">
4616 (90.1%)
</td>
<td style="text-align:left;">
509 (9.9%)
</td>
<td style="text-align:left;">
5125
</td>
</tr>
<tr grouplength="2">
<td colspan="4" style="border-bottom: 0;">
<strong>City</strong>
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
Hamilton
</td>
<td style="text-align:left;">
2307 (87.5%)
</td>
<td style="text-align:left;">
331 (12.5%)
</td>
<td style="text-align:left;">
2638
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
Vancouver
</td>
<td style="text-align:left;">
2309 (92.8%)
</td>
<td style="text-align:left;">
178 (7.2%)
</td>
<td style="text-align:left;">
2487
</td>
</tr>
<tr grouplength="24">
<td colspan="4" style="background-color: #666; color: #fff;">
<strong>Participant data</strong>
</td>
</tr>
<tr grouplength="2">
<td colspan="4" style="border-bottom: 0;">
<strong>BMI</strong>
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 4em;" indentlevel="2">
Mean (SD)
</td>
<td style="text-align:left;">
27.3 (5.4)
</td>
<td style="text-align:left;">
32.1 (6.4)
</td>
<td style="text-align:left;">
27.8 (5.7)
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 4em;" indentlevel="2">
Median (Q1; Q3)
</td>
<td style="text-align:left;">
26.4 (23.8; 29.8)
</td>
<td style="text-align:left;">
30.9 (27.5; 35.7)
</td>
<td style="text-align:left;">
26.8 (24.0; 30.4)
</td>
</tr>
<tr grouplength="2">
<td colspan="4" style="border-bottom: 0;">
<strong>Waist to hip ratio</strong>
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 4em;" indentlevel="2">
Mean (SD)
</td>
<td style="text-align:left;">
85.2 (9.0)
</td>
<td style="text-align:left;">
93.6 (8.5)
</td>
<td style="text-align:left;">
86.0 (9.3)
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 4em;" indentlevel="2">
Median (Q1; Q3)
</td>
<td style="text-align:left;">
85.2 (78.6; 91.6)
</td>
<td style="text-align:left;">
94.2 (87.8; 99.6)
</td>
<td style="text-align:left;">
86.0 (79.3; 92.6)
</td>
</tr>
<tr grouplength="2">
<td colspan="4" style="border-bottom: 0;">
<strong>Obesity (WHR)</strong>
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 4em;" indentlevel="2">
Low & Moderate
</td>
<td style="text-align:left;">
2787 (96.0%)
</td>
<td style="text-align:left;">
115 (4.0%)
</td>
<td style="text-align:left;">
2902
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 4em;" indentlevel="2">
High
</td>
<td style="text-align:left;">
1829 (82.3%)
</td>
<td style="text-align:left;">
394 (17.7%)
</td>
<td style="text-align:left;">
2223
</td>
</tr>
<tr grouplength="2">
<td colspan="4" style="border-bottom: 0;">
<strong>Age (years)</strong>
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 4em;" indentlevel="2">
Mean (SD)
</td>
<td style="text-align:left;">
10.5 (1.9)
</td>
<td style="text-align:left;">
11.4 (1.7)
</td>
<td style="text-align:left;">
10.6 (1.9)
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 4em;" indentlevel="2">
Median (Q1; Q3)
</td>
<td style="text-align:left;">
10.6 (9.0; 12.0)
</td>
<td style="text-align:left;">
11.6 (10.2; 12.6)
</td>
<td style="text-align:left;">
10.6 (9.2; 12.2)
</td>
</tr>
<tr grouplength="2">
<td colspan="4" style="border-bottom: 0;">
<strong>Sex</strong>
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 4em;" indentlevel="2">
Male
</td>
<td style="text-align:left;">
2076 (87.6%)
</td>
<td style="text-align:left;">
294 (12.4%)
</td>
<td style="text-align:left;">
2370
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 4em;" indentlevel="2">
Female
</td>
<td style="text-align:left;">
2540 (92.2%)
</td>
<td style="text-align:left;">
215 (7.8%)
</td>
<td style="text-align:left;">
2755
</td>
</tr>
<tr grouplength="6">
<td colspan="4" style="border-bottom: 0;">
<strong>Household Income Range</strong>
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 4em;" indentlevel="2">
&lt90k
</td>
<td style="text-align:left;">
1793 (93.3%)
</td>
<td style="text-align:left;">
129 (6.7%)
</td>
<td style="text-align:left;">
1922
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 4em;" indentlevel="2">
45k-65k
</td>
<td style="text-align:left;">
759 (89.8%)
</td>
<td style="text-align:left;">
86 (10.2%)
</td>
<td style="text-align:left;">
845
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 4em;" indentlevel="2">
30k-45k
</td>
<td style="text-align:left;">
564 (85.2%)
</td>
<td style="text-align:left;">
98 (14.8%)
</td>
<td style="text-align:left;">
662
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 4em;" indentlevel="2">
65k-90k
</td>
<td style="text-align:left;">
1010 (92.7%)
</td>
<td style="text-align:left;">
80 (7.3%)
</td>
<td style="text-align:left;">
1090
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 4em;" indentlevel="2">
20k-30k
</td>
<td style="text-align:left;">
304 (81.3%)
</td>
<td style="text-align:left;">
70 (18.7%)
</td>
<td style="text-align:left;">
374
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 4em;" indentlevel="2">
&lt20k
</td>
<td style="text-align:left;">
186 (80.2%)
</td>
<td style="text-align:left;">
46 (19.8%)
</td>
<td style="text-align:left;">
232
</td>
</tr>
<tr grouplength="2">
<td colspan="4" style="border-bottom: 0;">
<strong>AHEI Score</strong>
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 4em;" indentlevel="2">
Mean (SD)
</td>
<td style="text-align:left;">
3.8 (1.0)
</td>
<td style="text-align:left;">
3.5 (0.9)
</td>
<td style="text-align:left;">
3.7 (1.0)
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 4em;" indentlevel="2">
Median (Q1; Q3)
</td>
<td style="text-align:left;">
3.8 (3.1; 4.5)
</td>
<td style="text-align:left;">
3.5 (2.8; 4.1)
</td>
<td style="text-align:left;">
3.7 (3.0; 4.5)
</td>
</tr>
<tr grouplength="2">
<td colspan="4" style="border-bottom: 0;">
<strong>Physical Activity MET Score</strong>
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 4em;" indentlevel="2">
under
</td>
<td style="text-align:left;">
1721 (87.3%)
</td>
<td style="text-align:left;">
250 (12.7%)
</td>
<td style="text-align:left;">
1971
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 4em;" indentlevel="2">
over
</td>
<td style="text-align:left;">
2065 (92.3%)
</td>
<td style="text-align:left;">
172 (7.7%)
</td>
<td style="text-align:left;">
2237
</td>
</tr>
<tr grouplength="2">
<td colspan="4" style="border-bottom: 0;">
<strong>Ever smoked</strong>
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 4em;" indentlevel="2">
Mean (SD)
</td>
<td style="text-align:left;">
0.4 (0.5)
</td>
<td style="text-align:left;">
0.6 (0.5)
</td>
<td style="text-align:left;">
0.5 (0.5)
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 4em;" indentlevel="2">
Median (Q1; Q3)
</td>
<td style="text-align:left;">
0.0 (0.0; 1.0)
</td>
<td style="text-align:left;">
1.0 (0.0; 1.0)
</td>
<td style="text-align:left;">
0.0 (0.0; 1.0)
</td>
</tr>
<tr grouplength="2">
<td colspan="4" style="border-bottom: 0;">
<strong>Daily Drinker</strong>
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 4em;" indentlevel="2">
&gt=1 drinks/day
</td>
<td style="text-align:left;">
1157 (91.0%)
</td>
<td style="text-align:left;">
114 (9.0%)
</td>
<td style="text-align:left;">
1271
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 4em;" indentlevel="2">
&lt1 drink/day
</td>
<td style="text-align:left;">
2629 (89.5%)
</td>
<td style="text-align:left;">
308 (10.5%)
</td>
<td style="text-align:left;">
2937
</td>
</tr>
<tr grouplength="24">
<td colspan="4" style="background-color: #666; color: #fff;">
<strong>Census variables</strong>
</td>
</tr>
<tr grouplength="2">
<td colspan="4" style="border-bottom: 0;">
<strong>Neighbourhood type</strong>
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 4em;" indentlevel="2">
suburban/rural
</td>
<td style="text-align:left;">
3607 (89.9%)
</td>
<td style="text-align:left;">
405 (10.1%)
</td>
<td style="text-align:left;">
4012
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 4em;" indentlevel="2">
urban
</td>
<td style="text-align:left;">
1009 (90.7%)
</td>
<td style="text-align:left;">
104 (9.3%)
</td>
<td style="text-align:left;">
1113
</td>
</tr>
<tr grouplength="2">
<td colspan="4" style="border-bottom: 0;">
<strong>Individual mean income (CAD/1000)</strong>
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 4em;" indentlevel="2">
Mean (SD)
</td>
<td style="text-align:left;">
37.9 (13.9)
</td>
<td style="text-align:left;">
34.3 (12.1)
</td>
<td style="text-align:left;">
37.5 (13.8)
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 4em;" indentlevel="2">
Median (Q1; Q3)
</td>
<td style="text-align:left;">
35.0 (29.0; 42.0)
</td>
<td style="text-align:left;">
32.0 (27.0; 38.0)
</td>
<td style="text-align:left;">
35.0 (29.0; 42.0)
</td>
</tr>
<tr grouplength="2">
<td colspan="4" style="border-bottom: 0;">
<strong>Household median income (CAD/1000)</strong>
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 4em;" indentlevel="2">
Mean (SD)
</td>
<td style="text-align:left;">
65.5 (21.5)
</td>
<td style="text-align:left;">
60.6 (20.5)
</td>
<td style="text-align:left;">
65.0 (21.4)
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 4em;" indentlevel="2">
Median (Q1; Q3)
</td>
<td style="text-align:left;">
61.0 (51.0; 77.0)
</td>
<td style="text-align:left;">
57.0 (45.0; 71.0)
</td>
<td style="text-align:left;">
61.0 (51.0; 76.0)
</td>
</tr>
<tr grouplength="2">
<td colspan="4" style="border-bottom: 0;">
<strong>Prevalence of low income (%)</strong>
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 4em;" indentlevel="2">
Mean (SD)
</td>
<td style="text-align:left;">
9.4 (6.9)
</td>
<td style="text-align:left;">
11.3 (7.6)
</td>
<td style="text-align:left;">
9.6 (7.0)
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 4em;" indentlevel="2">
Median (Q1; Q3)
</td>
<td style="text-align:left;">
8.1 (4.3; 13.7)
</td>
<td style="text-align:left;">
10.1 (5.2; 16.3)
</td>
<td style="text-align:left;">
8.2 (4.4; 14.0)
</td>
</tr>
<tr grouplength="2">
<td colspan="4" style="border-bottom: 0;">
<strong>Commute Walking/Bicycle (%)</strong>
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 4em;" indentlevel="2">
Mean (SD)
</td>
<td style="text-align:left;">
7.5 (6.9)
</td>
<td style="text-align:left;">
6.8 (5.8)
</td>
<td style="text-align:left;">
7.4 (6.8)
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 4em;" indentlevel="2">
Median (Q1; Q3)
</td>
<td style="text-align:left;">
5.1 (3.0; 9.9)
</td>
<td style="text-align:left;">
5.3 (3.2; 8.6)
</td>
<td style="text-align:left;">
5.2 (3.0; 9.7)
</td>
</tr>
<tr grouplength="2">
<td colspan="4" style="border-bottom: 0;">
<strong>Labour force participation rate (%)</strong>
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 4em;" indentlevel="2">
Mean (SD)
</td>
<td style="text-align:left;">
66.6 (7.7)
</td>
<td style="text-align:left;">
65.3 (7.6)
</td>
<td style="text-align:left;">
66.5 (7.7)
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 4em;" indentlevel="2">
Median (Q1; Q3)
</td>
<td style="text-align:left;">
66.8 (61.8; 71.7)
</td>
<td style="text-align:left;">
64.9 (60.7; 70.5)
</td>
<td style="text-align:left;">
66.6 (61.7; 71.6)
</td>
</tr>
<tr grouplength="2">
<td colspan="4" style="border-bottom: 0;">
<strong>Gov’t transfer payments (%)</strong>
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 4em;" indentlevel="2">
Mean (SD)
</td>
<td style="text-align:left;">
9.9 (5.2)
</td>
<td style="text-align:left;">
12.4 (6.3)
</td>
<td style="text-align:left;">
10.1 (5.4)
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 4em;" indentlevel="2">
Median (Q1; Q3)
</td>
<td style="text-align:left;">
8.8 (5.7; 13.0)
</td>
<td style="text-align:left;">
11.5 (7.2; 16.1)
</td>
<td style="text-align:left;">
9.0 (5.8; 13.3)
</td>
</tr>
<tr grouplength="2">
<td colspan="4" style="border-bottom: 0;">
<strong>Unemployment rate (%)</strong>
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 4em;" indentlevel="2">
Mean (SD)
</td>
<td style="text-align:left;">
5.5 (2.5)
</td>
<td style="text-align:left;">
6.3 (2.9)
</td>
<td style="text-align:left;">
5.6 (2.6)
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 4em;" indentlevel="2">
Median (Q1; Q3)
</td>
<td style="text-align:left;">
5.4 (3.8; 7.0)
</td>
<td style="text-align:left;">
5.9 (4.4; 7.7)
</td>
<td style="text-align:left;">
5.4 (3.8; 7.0)
</td>
</tr>
<tr grouplength="2">
<td colspan="4" style="border-bottom: 0;">
<strong>Lone parent families (%)</strong>
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 4em;" indentlevel="2">
Mean (SD)
</td>
<td style="text-align:left;">
14.8 (6.5)
</td>
<td style="text-align:left;">
17.3 (7.5)
</td>
<td style="text-align:left;">
15.0 (6.6)
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 4em;" indentlevel="2">
Median (Q1; Q3)
</td>
<td style="text-align:left;">
14.0 (10.9; 18.4)
</td>
<td style="text-align:left;">
16.1 (11.9; 21.8)
</td>
<td style="text-align:left;">
14.1 (11.0; 18.8)
</td>
</tr>
<tr grouplength="2">
<td colspan="4" style="border-bottom: 0;">
<strong>Education - No degree (%)</strong>
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 4em;" indentlevel="2">
Mean (SD)
</td>
<td style="text-align:left;">
19.6 (9.2)
</td>
<td style="text-align:left;">
23.7 (10.4)
</td>
<td style="text-align:left;">
20.0 (9.4)
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 4em;" indentlevel="2">
Median (Q1; Q3)
</td>
<td style="text-align:left;">
17.7 (12.8; 24.9)
</td>
<td style="text-align:left;">
21.0 (15.8; 31.6)
</td>
<td style="text-align:left;">
17.9 (13.1; 25.6)
</td>
</tr>
<tr grouplength="2">
<td colspan="4" style="border-bottom: 0;">
<strong>Private Dwellings - Owned (%)</strong>
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 4em;" indentlevel="2">
Mean (SD)
</td>
<td style="text-align:left;">
73.9 (17.8)
</td>
<td style="text-align:left;">
72.9 (17.6)
</td>
<td style="text-align:left;">
73.8 (17.7)
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 4em;" indentlevel="2">
Median (Q1; Q3)
</td>
<td style="text-align:left;">
77.1 (62.1; 88.5)
</td>
<td style="text-align:left;">
75.6 (61.7; 87.0)
</td>
<td style="text-align:left;">
77.0 (62.1; 88.4)
</td>
</tr>
<tr grouplength="2">
<td colspan="4" style="border-bottom: 0;">
<strong>Private Dwellings - Rented (%)</strong>
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 4em;" indentlevel="2">
Mean (SD)
</td>
<td style="text-align:left;">
25.3 (17.2)
</td>
<td style="text-align:left;">
26.4 (17.0)
</td>
<td style="text-align:left;">
25.4 (17.2)
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 4em;" indentlevel="2">
Median (Q1; Q3)
</td>
<td style="text-align:left;">
22.1 (11.2; 36.9)
</td>
<td style="text-align:left;">
24.0 (12.9; 37.7)
</td>
<td style="text-align:left;">
22.4 (11.4; 36.9)
</td>
</tr>
<tr grouplength="8">
<td colspan="4" style="background-color: #666; color: #fff;">
<strong>Normalized Difference Vegetation Index (NDVI)</strong>
</td>
</tr>
<tr grouplength="2">
<td colspan="4" style="border-bottom: 0;">
<strong>NDVI - Median</strong>
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 4em;" indentlevel="2">
Mean (SD)
</td>
<td style="text-align:left;">
0.341 (0.089)
</td>
<td style="text-align:left;">
0.314 (0.084)
</td>
<td style="text-align:left;">
0.338 (0.089)
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 4em;" indentlevel="2">
Median (Q1; Q3)
</td>
<td style="text-align:left;">
0.336 (0.291; 0.379)
</td>
<td style="text-align:left;">
0.313 (0.266; 0.360)
</td>
<td style="text-align:left;">
0.333 (0.288; 0.377)
</td>
</tr>
<tr grouplength="2">
<td colspan="4" style="border-bottom: 0;">
<strong>NDVI - Standard Deviation</strong>
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 4em;" indentlevel="2">
Mean (SD)
</td>
<td style="text-align:left;">
0.090 (0.026)
</td>
<td style="text-align:left;">
0.088 (0.024)
</td>
<td style="text-align:left;">
0.090 (0.026)
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 4em;" indentlevel="2">
Median (Q1; Q3)
</td>
<td style="text-align:left;">
0.086 (0.071; 0.105)
</td>
<td style="text-align:left;">
0.084 (0.072; 0.100)
</td>
<td style="text-align:left;">
0.086 (0.071; 0.105)
</td>
</tr>
<tr grouplength="2">
<td colspan="4" style="border-bottom: 0;">
<strong>NDVI - Min</strong>
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 4em;" indentlevel="2">
Mean (SD)
</td>
<td style="text-align:left;">
0.206 (0.097)
</td>
<td style="text-align:left;">
0.182 (0.086)
</td>
<td style="text-align:left;">
0.204 (0.096)
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 4em;" indentlevel="2">
Median (Q1; Q3)
</td>
<td style="text-align:left;">
0.202 (0.135; 0.262)
</td>
<td style="text-align:left;">
0.173 (0.118; 0.243)
</td>
<td style="text-align:left;">
0.198 (0.134; 0.260)
</td>
</tr>
<tr grouplength="2">
<td colspan="4" style="border-bottom: 0;">
<strong>NDVI - max</strong>
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 4em;" indentlevel="2">
Mean (SD)
</td>
<td style="text-align:left;">
0.495 (0.093)
</td>
<td style="text-align:left;">
0.465 (0.093)
</td>
<td style="text-align:left;">
0.492 (0.093)
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 4em;" indentlevel="2">
Median (Q1; Q3)
</td>
<td style="text-align:left;">
0.484 (0.435; 0.546)
</td>
<td style="text-align:left;">
0.459 (0.401; 0.514)
</td>
<td style="text-align:left;">
0.482 (0.432; 0.543)
</td>
</tr>
</tbody>
</table>

# About

### Package contributors

Brinkmann, Sebastian Tobias (Package creator and author) e-mail:
<sebastian.brinkmann@fau.de> Große, Tim (Contributor)

### Thesis authors

Walker, Blake Byron (1\*)  
Brinkmann, Sebastian Tobias (1)  
Große, Tim (1)  
Dominik Kremer (1)  
Schuurman Nadine (2)  
Hystad Perry (3)  
Rangarajan Sumathy (4)  
Teo Koon (4)  
Yusuf Salim (4)  
Lear Scott A. (5)

1: Community Health Environments and Social Terrains (CHEST) Lab,
Institut für Geographie, Friedrich-Alexander-Universität
Erlangen-Nürnberg, Wetterkreuz 15, 91052 Erlangen, Germany

\*corresponding author

2: Department of Geography, Simon Fraser University, Burnaby, Canada

3: Spatial Health Lab, College of Public Health and Human Sciences,
Oregon State University, Corvallis, USA

4: Population Health Research Institute, McMaster University, Hamilton,
Canada

5: Faculty of Health Sciences, Simon Fraser University, Burnaby, Canada

# Bibliography

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-Bauer.2016" class="csl-entry">

Bauer, Jan, and David A. Groneberg. 2016. “Measuring Spatial
Accessibility of Health Care Providers - Introduction of a Variable
Distance Decay Function Within the Floating Catchment Area (FCA)
Method.” *PloS One* 11 (7): e0159148.
<https://doi.org/10.1371/journal.pone.0159148>.

</div>

<div id="ref-Fuertes.2014" class="csl-entry">

Fuertes, Elaine, Iana Markevych, Andrea von Berg, Carl-Peter Bauer,
Dietrich Berdel, Sibylle Koletzko, Dorothea Sugiri, and Joachim
Heinrich. 2014. “Greenness and Allergies: Evidence of Differential
Associations in Two Areas in Germany.” *Journal of Epidemiology and
Community Health* 68 (8): 787–90.
<https://doi.org/10.1136/jech-2014-203903>.

</div>

<div id="ref-Gong.2014" class="csl-entry">

Gong, Yi, John Gallacher, Stephen Palmer, and David Fone. 2014.
“Neighbourhood Green Space, Physical Function and Participation in
Physical Activities Among Elderly Men: The Caerphilly Prospective
Study.” *International Journal of Behavioral Nutrition and Physical
Activity* 11 (1): 40. <https://doi.org/10.1186/1479-5868-11-40>.

</div>

<div id="ref-Hart.1968" class="csl-entry">

Hart, Peter, Nils Nilsson, and Bertram Raphael. 1968. “A Formal Basis
for the Heuristic Determination of Minimum Cost Paths.” *IEEE
Transactions on Systems Science and Cybernetics* 4 (2): 100–107.
<https://doi.org/10.1109/TSSC.1968.300136>.

</div>

<div id="ref-Jia.2019" class="csl-entry">

Jia, Peng, Fahui Wang, and Imam M. Xierali. 2019. “Differential Effects
of Distance Decay on Hospital Inpatient Visits Among Subpopulations in
Florida, USA.” *Environmental Monitoring and Assessment* 191 (Suppl 2):
381. <https://doi.org/10.1007/s10661-019-7468-2>.

</div>

<div id="ref-MichaelDorman.2020" class="csl-entry">

Michael Dorman. 2020. “Nngeo: K-Nearest Neighbor Join for Spatial Data.”
<https://CRAN.R-project.org/package=nngeo>.

</div>

<div id="ref-Padgham.2017" class="csl-entry">

Padgham, Mark, Robin Lovelace, Maëlle Salmon, and Bob Rudis. 2017.
“Osmdata.” *Journal of Open Source Software* 2 (14): 305.
<https://doi.org/10.21105/joss.00305>.

</div>

<div id="ref-Scarpone.2020" class="csl-entry">

Scarpone, Christopher, Sebastian T. Brinkmann, Tim Große, Daniel
Sonnenwald, Martin Fuchs, and Blake Byron Walker. 2020. “A Multimethod
Approach for County-Scale Geospatial Analysis of Emerging Infectious
Diseases: A Cross-Sectional Case Study of COVID-19 Incidence in
Germany.” *International Journal of Health Geographics* 19 (1): 32.
<https://doi.org/10.1186/s12942-020-00225-1>.

</div>

<div id="ref-Walker_2022" class="csl-entry">

Walker, Blake Byron, Sebastian Tobias Brinkmann, Tim Große, Dominik
Kremer, Nadine Schuurman, Perry Hystad, Sumathy Rangarajan, Koon Teo,
Salim Yusuf, and Scott A. Lear. 2022. “Neighborhood Greenspace and
Socioeconomic Risk Are Associated with Diabetes Risk at the
Sub-Neighborhood Scale: Results from the Prospective Urban and Rural
Epidemiology (PURE) Study.” *Journal of Urban Health* 99 (3): 506–18.
<https://doi.org/10.1007/s11524-022-00630-w>.

</div>

<div id="ref-Walker.2019" class="csl-entry">

Walker, Blake Byron, Aateka Shashank, Danijela Gasevic, Nadine
Schuurman, Paul Poirier, Koon Teo, Sumathy Rangarajan, Salim Yusuf, and
Scott A. Lear. 2019. “The Local Food Environment and Obesity: Evidence
from Three Cities.” *Obesity (Silver Spring, Md.)* 28 (1): 40–45.
<https://doi.org/10.1002/oby.22614>.

</div>

</div>
