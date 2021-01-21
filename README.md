DRI-GLUCoSE — Work in Progress
================

  - [Installation](#installation)
  - [Methods](#methods)
      - [Census variables](#census-variables)
      - [Greenspace](#greenspace)
      - [Exposure Model](#exposure-model)
          - [(i) Road network data and
            isochrones](#i-road-network-data-and-isochrones)
          - [(ii) Distance-weighting](#ii-distance-weighting)
  - [Appendix](#appendix)
      - [Tables](#tables)
      - [Figures](#figures)
      - [Summary Statistics](#summary-statistics)
  - [About](#about)
      - [Package contributors:](#package-contributors)
      - [Thesis authors:](#thesis-authors)
  - [Bibliography](#bibliography)

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

# Installation

You can install the latest version of DRIGLUCoSE from
[GitHub](https://CRAN.R-project.org) with:

``` r
devtools::install_git("https://github.com/STBrinkmann/DRIGLUCoSE")
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
#> Simple feature collection with 2 features and 2 fields
#> geometry type:  POINT
#> dimension:      XY
#> bbox:           xmin: 35199.46 ymin: -159433.5 xmax: 36281.59 ymax: -159243.2
#> projected CRS:  ETRS89 / LCC Germany (N-E)
#> # A tibble: 2 x 3
#>     tag Speed                 geom
#>   <dbl> <dbl>          <POINT [m]>
#> 1     1  78.5 (35199.46 -159433.5)
#> 2     2  79.8 (36281.59 -159243.2)
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
#> Simple feature collection with 2142 features and 3 fields
#> geometry type:  POLYGON
#> dimension:      XY
#> bbox:           xmin: 33236.96 ymin: -161396 xmax: 38336.96 ymax: -157196
#> projected CRS:  ETRS89 / LCC Germany (N-E)
#> First 10 features:
#>    census_var_a census_var_b census_var_c                           geom
#> 1           284         2009       114811 POLYGON ((33236.96 -161396,...
#> 2           848         3318       139961 POLYGON ((33336.96 -161396,...
#> 3           918         3954       112673 POLYGON ((33436.96 -161396,...
#> 4           101         3359       137505 POLYGON ((33536.96 -161396,...
#> 5           623         3107       109201 POLYGON ((33636.96 -161396,...
#> 6           905         2630       131656 POLYGON ((33736.96 -161396,...
#> 7           645         6512       135789 POLYGON ((33836.96 -161396,...
#> 8           934         9945       115006 POLYGON ((33936.96 -161396,...
#> 9           400         3583       111379 POLYGON ((34036.96 -161396,...
#> 10          900         7778       137576 POLYGON ((34136.96 -161396,...
```

## Greenspace

In our analysis we acquired LANDSAT images through the United States
Geological Survey’s EarthExplorer platform
(<https://earthexplorer.usgs.gov/>). The Normalized Difference
Vegetation Index
([NDVI](https://gisgeography.com/ndvi-normalized-difference-vegetation-index/))
is used as a metric to model greenspace exposure. Pre-processing of the
LANDSAT images and NDVI calculation has been conducted using the
*LS\_L1C* function:

``` r
DRIGLUCoSE::LS_L1C(l1c_path = "docs/LC08_L1TP_193026_20200423_20200508_01_T1_small/", 
                   out_dir = "docs/LS_PreProcessed",
                   # Use Sample Data and apply 25 minutes buffer (Speed[m/min] * 25[min]) 
                   sf_mask = DRIGLUCoSE::Erlangen %>% 
                     dplyr::mutate(geom = sf::st_buffer(geom, Speed*25)),
                   cores = 20)
#> Project raster
#> DN to TOA Reflectance
#> class      : RasterStack 
#> dimensions : 122, 151, 18422, 8  (nrow, ncol, ncell, nlayers)
#> resolution : 30, 30  (x, y)
#> extent     : 33493.69, 38023.69, -161164.2, -157504.2  (xmin, xmax, ymin, ymax)
#> crs        : +proj=lcc +lat_0=51 +lon_0=10.5 +lat_1=48.6666666666667 +lat_2=53.6666666666667 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs 
#> names      :      Blue,     Green,       Red,       NIR,     SWIR1,     SWIR2,      NDWI,      NDVI 
#> min values :         0,         0,         0,         0,         0,         0,        -1,        -1 
#> max values : 0.2020575, 0.2322532, 0.3076383, 0.5424371, 0.4233773, 0.3753066, 1.0000000, 1.0000000
```

## Exposure Model

In order to estimate each participant’s potential exposures to
greenspace and local SES, we (i) mapped age- and sex-specific walkable
zones around their residential address, and (ii) applied a negative
logit weighting function, such that the estimated effect of greenspace
or SES decreases as distance from the home increases.

### (i) Road network data and isochrones

In order to compute network-based distance metrics, we acquired street
data from OpenStreetMap using the R-package *osmdata* (Padgham et
al. 2017). Road types not suitable for walking were removed (e.g.,
motorways). Network data were topologically corrected and split into
\~20 metre-long segments using the R package *nngeo* (Dorman 2020).

``` r
erlangen.osm <- DRIGLUCoSE::osm_roads(x = Erlangen, dist = 20, 
                                      speed = "Speed", cores = 2)
```

This network data was used to derive walking distance buffers for each
participant, based on walking speed. Starting from each participant’s
place of residence, we computed network-constrained buffers with an
off-road width of 40 meters, running in 2-minute increments from 0 to 20
minutes, using the A\*-algorithm (Hart, Nilsson & Raphael 1968). This
therefore resulted in each participant having ten concentric isochrones,
the sizes of which are a function of individual walking speed and road
network.  
Since the road network contains a lot of features (n=14420), this will
take some time (\~15-30 minutes).

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
#> Simple feature collection with 20 features and 2 fields
#> geometry type:  POLYGON
#> dimension:      XY
#> bbox:           xmin: 34033.6 ymin: -160836.2 xmax: 37706.61 ymax: -157758.7
#> projected CRS:  ETRS89 / LCC Germany (N-E)
#> # A tibble: 20 x 3
#>      tag time                                                               geom
#>  * <dbl> <chr>                                                     <POLYGON [m]>
#>  1     1 2     ((35069.53 -159364.5, 35069.53 -159364.1, 35069.56 -159363.5, 35~
#>  2     1 4     ((34939.85 -159373.8, 34940.23 -159373.4, 34940.61 -159372.9, 34~
#>  3     1 6     ((34892.51 -159333.3, 34892.51 -159332.4, 34892.52 -159332.3, 34~
#>  4     1 8     ((34892.51 -159333.3, 34892.51 -159332.4, 34892.52 -159332.3, 34~
#>  5     1 10    ((34832.45 -159080.6, 34832.44 -159080, 34832.45 -159079.7, 3483~
#>  6     1 12    ((34609.81 -159190.6, 34607.8 -159191.1, 34605.76 -159191.6, 346~
#>  7     1 14    ((34491.75 -159216.3, 34491.53 -159216.4, 34489.45 -159216.6, 34~
#>  8     1 16    ((34314.4 -159277.6, 34312.43 -159278.3, 34310.41 -159278.9, 343~
#>  9     1 18    ((34137.69 -159298.2, 34136.14 -159296.8, 34134.67 -159295.3, 34~
#> 10     1 20    ((34033.61 -159310, 34033.6 -159309.9, 34033.63 -159308.9, 34033~
#> 11     2 2     ((36169.37 -159274.8, 36169.3 -159274.8, 36143.99 -159271.6, 361~
#> 12     2 4     ((35937.49 -159174.3, 35937.53 -159174.1, 35937.8 -159173.3, 359~
#> 13     2 6     ((35842.74 -159227.6, 35841.66 -159227.5, 35839.58 -159227.3, 35~
#> 14     2 8     ((35660.08 -159105.1, 35659.91 -159105.1, 35654.15 -159102.5, 35~
#> 15     2 10    ((36942.64 -159679.2, 36942.96 -159679.6, 36944.31 -159681.2, 36~
#> 16     2 12    ((35368.96 -158950.9, 35368.23 -158950.5, 35366.45 -158949.4, 35~
#> 17     2 14    ((35212.62 -158969.4, 35212.55 -158968.6, 35212.47 -158966.6, 35~
#> 18     2 16    ((35503.8 -159156.6, 35503.85 -159156.6, 35504.41 -159157.4, 355~
#> 19     2 18    ((34982.31 -158901.1, 34982.26 -158899, 34982.31 -158896.9, 3498~
#> 20     2 20    ((34832.45 -159080.6, 34832.44 -159080, 34832.45 -159079.7, 3483~
```

Figure 1 shows isodistances of the two points of the sample data in
Erlangen, Germany.

<img src="man/figures/README-unnamed-chunk-11-1.svg" width="100%" />

### (ii) Distance-weighting

In order to account for the diminishing effect of SES and greenspace
exposure as distance increases, we fitted a logit function to weight
each incremental isochrone, such that the influence of a variable
decreases with increasing distance from the household, i.e., features
that are farther away have less influence than nearby features, as
illustrated in Figure 2. A logit function was selected as it
heuristically approximates a suitable distance-decay function (Bauer and
Groneberg 2016; Jia et al. 2019).  
The distance-weighting is separated in two parts, first the logit
function (1) that is used for both SES and greenspace variables, and
second the proportional weights function (4) that is only applied on SES
variables.

  
![&#10;\\begin{align\*}&#10; G\_t =&#10; \\begin{cases}&#10;
\\cfrac{\\int\_0^{r\_t} \\, g(r)dr}{\\int\_0^{r\_{t\_{max}}} \\,
g(r)dr}, t=1\\\\&#10; \\cfrac{\\int\_{r\_{t-1}}^{r\_t} \\,
g(r)dr}{\\int\_0^{r\_{t\_{max}}} \\, g(r)dr}, t\>1&#10;
\\end{cases}&#10; &&
\\text{(1)}&#10;\\end{align\*}&#10;](https://latex.codecogs.com/svg.latex?%0A%5Cbegin%7Balign%2A%7D%0A%20%20G_t%20%3D%0A%20%20%20%20%5Cbegin%7Bcases%7D%0A%20%20%20%20%20%20%5Ccfrac%7B%5Cint_0%5E%7Br_t%7D%20%5C%2C%20g%28r%29dr%7D%7B%5Cint_0%5E%7Br_%7Bt_%7Bmax%7D%7D%7D%20%5C%2C%20g%28r%29dr%7D%2C%20t%3D1%5C%5C%0A%20%20%20%20%20%20%5Ccfrac%7B%5Cint_%7Br_%7Bt-1%7D%7D%5E%7Br_t%7D%20%20%5C%2C%20g%28r%29dr%7D%7B%5Cint_0%5E%7Br_%7Bt_%7Bmax%7D%7D%7D%20%5C%2C%20g%28r%29dr%7D%2C%20t%3E1%0A%20%20%20%20%5Cend%7Bcases%7D%0A%20%20%20%20%26%26%20%5Ctext%7B%281%29%7D%0A%5Cend%7Balign%2A%7D%0A
"
\\begin{align*}
  G_t =
    \\begin{cases}
      \\cfrac{\\int_0^{r_t} \\, g(r)dr}{\\int_0^{r_{t_{max}}} \\, g(r)dr}, t=1\\\\
      \\cfrac{\\int_{r_{t-1}}^{r_t}  \\, g(r)dr}{\\int_0^{r_{t_{max}}} \\, g(r)dr}, t\>1
    \\end{cases}
    && \\text{(1)}
\\end{align*}
")  

Each isochrone ![t](https://latex.codecogs.com/svg.latex?t "t") is
assigned a distance weight
![G\_T](https://latex.codecogs.com/svg.latex?G_T "G_T"), calculated as
the integral of the logistic distance decay function
![g(r)](https://latex.codecogs.com/svg.latex?g%28r%29 "g(r)") (2)

  
![&#10;\\begin{align\*}&#10; g(r) =&#10; \\cfrac{1}{1 + e^{ \\,b
\\,(r-m)}}&#10; &&
\\text{(2)}&#10;\\end{align\*}&#10;](https://latex.codecogs.com/svg.latex?%0A%5Cbegin%7Balign%2A%7D%0A%20%20g%28r%29%20%3D%0A%20%20%20%20%5Ccfrac%7B1%7D%7B1%20%2B%20e%5E%7B%20%5C%2Cb%20%5C%2C%28r-m%29%7D%7D%0A%20%20%20%20%26%26%20%5Ctext%7B%282%29%7D%0A%5Cend%7Balign%2A%7D%0A
"
\\begin{align*}
  g(r) =
    \\cfrac{1}{1 + e^{ \\,b \\,(r-m)}}
    && \\text{(2)}
\\end{align*}
")  

with ![b = 8](https://latex.codecogs.com/svg.latex?b%20%3D%208 "b = 8")
and ![m = 0.6](https://latex.codecogs.com/svg.latex?m%20%3D%200.6
"m = 0.6"), in the interval between the mean inner radius
![r\_{t-1}](https://latex.codecogs.com/svg.latex?r_%7Bt-1%7D "r_{t-1}")
and mean outer radius ![r\_t](https://latex.codecogs.com/svg.latex?r_t
"r_t") of the isochrone (e.g. 2 to 4 minutes isochrones), normalized by
the integral from 0 to the outermost isochrone boundary
![r\_{t\_{max}}](https://latex.codecogs.com/svg.latex?r_%7Bt_%7Bmax%7D%7D
"r_{t_{max}}") (e.g. 20 minutes isochrone). Weighted summary statistics
to describe the greenspace (e.g. mean or minimum NDVI) are thus
described as (3)

  
![&#10;\\begin{align\*}&#10; \\sum\_t G\_t \\, f(NDVI\_t \\, \\cap \\,
I\_t)&#10; &&
\\text{(3)}&#10;\\end{align\*}&#10;](https://latex.codecogs.com/svg.latex?%0A%5Cbegin%7Balign%2A%7D%0A%20%20%5Csum_t%20G_t%20%5C%2C%20f%28NDVI_t%20%5C%2C%20%5Ccap%20%5C%2C%20I_t%29%0A%20%20%20%20%26%26%20%5Ctext%7B%283%29%7D%0A%5Cend%7Balign%2A%7D%0A
"
\\begin{align*}
  \\sum_t G_t \\, f(NDVI_t \\, \\cap \\, I_t)
    && \\text{(3)}
\\end{align*}
")  

For SES variables the proportional weights of the census areas within
the isochrone are further defined as (4)

  
![&#10;\\begin{align\*}&#10; A\_{tj} =&#10; \\cfrac{A(C\_j \\, \\cap \\,
I\_t)}&#10; {A(I\_t)}&#10; &&
\\text{(4)}&#10;\\end{align\*}&#10;](https://latex.codecogs.com/svg.latex?%0A%5Cbegin%7Balign%2A%7D%0A%20%20A_%7Btj%7D%20%3D%0A%20%20%20%20%5Ccfrac%7BA%28C_j%20%5C%2C%20%5Ccap%20%5C%2C%20I_t%29%7D%0A%20%20%20%20%7BA%28I_t%29%7D%0A%20%20%20%20%26%26%20%5Ctext%7B%284%29%7D%0A%5Cend%7Balign%2A%7D%0A
"
\\begin{align*}
  A_{tj} =
    \\cfrac{A(C_j \\, \\cap \\, I_t)}
    {A(I_t)}
    && \\text{(4)}
\\end{align*}
")  

with the proportion of the area of the intersection of the census area
![C\_j](https://latex.codecogs.com/svg.latex?C_j "C_j") and the
isochrone ![I\_t](https://latex.codecogs.com/svg.latex?I_t "I_t"), and
the area of the isochrone
![I\_t](https://latex.codecogs.com/svg.latex?I_t "I_t"). The weighted
value of the SES variable
![x\_i](https://latex.codecogs.com/svg.latex?x_i "x_i") in the census
area ![j](https://latex.codecogs.com/svg.latex?j "j") is then defined as
(5)

  
![&#10;\\begin{align\*}&#10; \\sum\_t \\left( \\ G\_t \\ \\sum\_j \\,
x\_{ij} \\; A{tj} \\right)&#10; &&
\\text{(5)}&#10;\\end{align\*}&#10;](https://latex.codecogs.com/svg.latex?%0A%5Cbegin%7Balign%2A%7D%0A%20%20%5Csum_t%20%5Cleft%28%20%5C%20G_t%20%5C%20%5Csum_j%20%5C%2C%20x_%7Bij%7D%20%5C%3B%20A%7Btj%7D%20%5Cright%29%0A%20%20%20%20%26%26%20%5Ctext%7B%285%29%7D%0A%5Cend%7Balign%2A%7D%0A
"
\\begin{align*}
  \\sum_t \\left( \\ G_t \\ \\sum_j \\, x_{ij} \\; A{tj} \\right)
    && \\text{(5)}
\\end{align*}
")  

Figure 2 visualizes the different submodels used for distance-weighting
SES and greenspace. Fig. 2a shows the unweighted values of a SES
variable and fig. 2b has been calculated using (5), thus representing
the proportional weights of all intersections with the census areas and
isochrones. Greenspace is weighted as shown in fig. 2c using (3).

<div class="figure">

<img src="docs/WeightsPlot.svg" alt="Figure 2: Unweighted values (a) and network-based distance-weighting function for socioeconomic variables (b) and greenspace (c). Bold black lines indicate the isochrones." width="90%" />

<p class="caption">

Figure 2: Unweighted values (a) and network-based distance-weighting
function for socioeconomic variables (b) and greenspace (c). Bold black
lines indicate the isochrones.

</p>

</div>

  
  
The distance-weighting for the LANDSAT derived NDVI raster (greenspace
exposure) is handled using *LS\_band\_weighting*, and SES distance- and
areal-weighting using *census\_weighting*.

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
                                b = 8, m = 0.6)

NDVI_weighted
#> # A tibble: 2 x 6
#>     tag    sd median X5_percentile X95_percentile    skew
#>   <dbl> <dbl>  <dbl>         <dbl>          <dbl>   <dbl>
#> 1     1 0.204  0.597         0.261          0.906 -0.133 
#> 2     2 0.143  0.561         0.321          0.790 -0.0302
```

``` r
census_weighted <- DRIGLUCoSE::census_weighting(isochrones = erlangen.isochrones, 
                                                tag = "tag", census = census, 
                                                b = 8, m = 0.6)
census_weighted
#> # A tibble: 2 x 4
#>     tag census_var_a census_var_b census_var_c
#>   <dbl>        <dbl>        <dbl>        <dbl>
#> 1     1         522.        5215.      128452.
#> 2     2         521.        5477.      124588.
```

# Appendix

## Tables

<table style='width:90%; font-family: "Arial Narrow", "Source Sans Pro", sans-serif; margin-left: auto; margin-right: auto;' class=" lightable-classic lightable-striped">

<caption>

Table A.1: Logistic models for all multivariable models with odds
ratios, 95% CI, and p-values.

</caption>

<thead>

<tr>

<th style="empty-cells: hide;" colspan="2">

</th>

<th style="padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="2">

<div style="TRUE">

Semi-Adjusted

</div>

</th>

<th style="padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="2">

<div style="TRUE">

Fully-Adjusted

</div>

</th>

</tr>

<tr>

<th style="text-align:left;">

Parameter

</th>

<th style="text-align:center;">

OR<br>(bivariate)

</th>

<th style="text-align:center;">

OR<br>(WHR-adjusted)

</th>

<th style="text-align:center;">

OR<br>(BMI-adjusted)

</th>

<th style="text-align:center;">

OR<br>(WHR-adjusted)

</th>

<th style="text-align:center;">

OR<br>(BMI-adjusted)

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

DRI-GLUCoSE Score

</td>

<td style="text-align:center;">

0.4 (0.3-0.54,<br>p-value\<0.001)

</td>

<td style="text-align:center;">

0.55 (0.43-0.7,<br>p-value\<0.001)

</td>

<td style="text-align:center;">

0.59 (0.46-0.76,<br>p-value\<0.001)

</td>

<td style="text-align:center;">

0.5 (0.38-0.64,<br>p-value\<0.001)

</td>

<td style="text-align:center;">

0.57 (0.43-0.75,<br>p-value\<0.001)

</td>

</tr>

<tr>

<td style="text-align:left;">

Age (5 year-interval)

</td>

<td style="text-align:center;">

1.27 (1.19-1.36,<br>p-value\<0.001)

</td>

<td style="text-align:center;">

1.2 (1.14-1.26,<br>p-value\<0.001)

</td>

<td style="text-align:center;">

1.26 (1.2-1.32,<br>p-value\<0.001)

</td>

<td style="text-align:center;">

1.19 (1.13-1.26,<br>p-value\<0.001)

</td>

<td style="text-align:center;">

1.23 (1.17-1.3,<br>p-value\<0.001)

</td>

</tr>

<tr>

<td style="text-align:left;">

Sex: female

</td>

<td style="text-align:center;">

0.61 (0.48-0.76,<br>p-value\<0.001)

</td>

<td style="text-align:center;">

0.89 (0.74-1.06,<br>p-value=0.19)

</td>

<td style="text-align:center;">

0.46 (0.38-0.54,<br>p-value\<0.001)

</td>

<td style="text-align:center;">

0.83 (0.67-1.02,<br>p-value=0.078)

</td>

<td style="text-align:center;">

0.43 (0.35-0.52,<br>p-value\<0.001)

</td>

</tr>

<tr>

<td style="text-align:left;">

Obese (WHR)

</td>

<td style="text-align:center;">

5.54 (4.24-7.33,<br>p-value\<0.001)

</td>

<td style="text-align:center;">

5.96 (4.97-7.16,<br>p-value\<0.001)

</td>

<td style="text-align:center;">

</td>

<td style="text-align:center;">

5.02 (4.08-6.19,<br>p-value\<0.001)

</td>

<td style="text-align:center;">

</td>

</tr>

<tr>

<td style="text-align:left;">

BMI

</td>

<td style="text-align:center;">

1.14 (1.12-1.16,<br>p-value\<0.001)

</td>

<td style="text-align:center;">

</td>

<td style="text-align:center;">

1.15 (1.13-1.17,<br>p-value\<0.001)

</td>

<td style="text-align:center;">

</td>

<td style="text-align:center;">

1.14 (1.12-1.16,<br>p-value\<0.001)

</td>

</tr>

<tr>

<td style="text-align:left;">

Household income range

</td>

<td style="text-align:center;">

0.74 (0.69-0.8,<br>p-value\<0.001)

</td>

<td style="text-align:center;">

0.87 (0.82-0.92,<br>p-value\<0.001)

</td>

<td style="text-align:center;">

0.85 (0.8-0.9,<br>p-value\<0.001)

</td>

<td style="text-align:center;">

0.84 (0.79-0.9,<br>p-value\<0.001)

</td>

<td style="text-align:center;">

0.82 (0.77-0.88,<br>p-value\<0.001)

</td>

</tr>

<tr>

<td style="text-align:left;">

Neighbourhood type: urban

</td>

<td style="text-align:center;">

0.88 (0.67-1.16,<br>p-value=0.385)

</td>

<td style="text-align:center;">

0.69 (0.56-0.86,<br>p-value\<0.001)

</td>

<td style="text-align:center;">

0.72 (0.58-0.9,<br>p-value=0.003)

</td>

<td style="text-align:center;">

0.54 (0.42-0.69,<br>p-value\<0.001)

</td>

<td style="text-align:center;">

0.64 (0.5-0.82,<br>p-value\<0.001)

</td>

</tr>

<tr>

<td style="text-align:left;">

AHEI Score (E^1)

</td>

<td style="text-align:center;">

0.76 (0.68-0.85,<br>p-value\<0.001)

</td>

<td style="text-align:center;">

</td>

<td style="text-align:center;">

</td>

<td style="text-align:center;">

0.87 (0.79-0.96,<br>p-value=0.005)

</td>

<td style="text-align:center;">

0.92 (0.84-1.02,<br>p-value=0.126)

</td>

</tr>

<tr>

<td style="text-align:left;">

Physical Activity MET Score

</td>

<td style="text-align:center;">

0.74 (0.63-0.88,<br>p-value\<0.001)

</td>

<td style="text-align:center;">

</td>

<td style="text-align:center;">

</td>

<td style="text-align:center;">

0.9 (0.78-1.03,<br>p-value=0.134)

</td>

<td style="text-align:center;">

0.92 (0.8-1.06,<br>p-value=0.272)

</td>

</tr>

<tr>

<td style="text-align:left;">

Current/Former smoker: yes

</td>

<td style="text-align:center;">

1.66 (1.32-2.09,<br>p-value\<0.001)

</td>

<td style="text-align:center;">

</td>

<td style="text-align:center;">

</td>

<td style="text-align:center;">

1.43 (1.18-1.74,<br>p-value\<0.001)

</td>

<td style="text-align:center;">

1.27 (1.04-1.54,<br>p-value=0.018)

</td>

</tr>

<tr>

<td style="text-align:left;">

Alcohol: \<1 drink/day

</td>

<td style="text-align:center;">

1.19 (0.92-1.54,<br>p-value=0.184)

</td>

<td style="text-align:center;">

</td>

<td style="text-align:center;">

</td>

<td style="text-align:center;">

2.11 (1.69-2.64,<br>p-value\<0.001)

</td>

<td style="text-align:center;">

1.81 (1.45-2.26,<br>p-value\<0.001)

</td>

</tr>

</tbody>

</table>

  

<table style='width:80%; font-family: "Arial Narrow", "Source Sans Pro", sans-serif; margin-left: auto; margin-right: auto;' class=" lightable-classic">

<caption>

Table A.2: Model Performance for all multivariable models.

</caption>

<thead>

<tr>

<th style="empty-cells: hide;" colspan="1">

</th>

<th style="padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="2">

<div style="border-bottom: 1px solid #111111; margin-bottom: -1px; ">

Semi-Adjusted

</div>

</th>

<th style="padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="2">

<div style="border-bottom: 1px solid #111111; margin-bottom: -1px; ">

Fully-Adjusted

</div>

</th>

</tr>

<tr>

<th style="text-align:left;">

Metric

</th>

<th style="text-align:center;">

OR<br>(WHR-adjusted)

</th>

<th style="text-align:center;">

OR<br>(BMI-adjusted)

</th>

<th style="text-align:center;">

OR<br>(WHR-adjusted)

</th>

<th style="text-align:left;">

OR<br>(BMI-adjusted)

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Sensitivity

</td>

<td style="text-align:center;">

0.67

</td>

<td style="text-align:center;">

0.76

</td>

<td style="text-align:center;">

0.69

</td>

<td style="text-align:left;">

0.78

</td>

</tr>

<tr>

<td style="text-align:left;">

Specificity

</td>

<td style="text-align:center;">

0.71

</td>

<td style="text-align:center;">

0.64

</td>

<td style="text-align:center;">

0.72

</td>

<td style="text-align:left;">

0.66

</td>

</tr>

<tr>

<td style="text-align:left;">

Youden index

</td>

<td style="text-align:center;">

0.38

</td>

<td style="text-align:center;">

0.41

</td>

<td style="text-align:center;">

0.41

</td>

<td style="text-align:left;">

0.44

</td>

</tr>

</tbody>

</table>

  

## Figures

<div class="figure">

<img src="docs/forest_plot_whr.svg" alt="Figure A.1: Forest plot showing significant effects for both BMI- and WHR-controlled multivariable logistic models." width="90%" />

<p class="caption">

Figure A.1: Forest plot showing significant effects for both BMI- and
WHR-controlled multivariable logistic models.

</p>

</div>

  

<div class="figure">

<img src="docs/roc_auc.svg" alt="Figure A.2: The ROC curves for both BMI- and WHR-controlled multivariable logistic models." width="90%" />

<p class="caption">

Figure A.2: The ROC curves for both BMI- and WHR-controlled
multivariable logistic models.

</p>

</div>

## Summary Statistics

``` r
readr::read_delim("docs/summary_statistiks.csv", delim = ";") %>% 
  kableExtra::kbl(format = "html", escape = FALSE, caption = "Table A.3: Baseline characteristics of the study population by diabetes status.") %>% 
  kable_paper("striped", full_width = F) %>%
  pack_rows("City", 2, 3) %>%
  pack_rows("Participant data", 4, 27) %>%
  pack_rows("BMI", 4, 5) %>%
  pack_rows("Waist to hip ratio", 6, 7) %>%
  pack_rows("Obesity (WHR)", 8, 9) %>%
  pack_rows("Age (years)", 10, 11) %>%
  pack_rows("Sex", 12, 13) %>%
  pack_rows("Household Income Range", 14, 19) %>%
  pack_rows("AHEI Score", 20, 21) %>%
  pack_rows("Physical Activity MET Score", 22, 23) %>%
  pack_rows("Ever smoked", 24, 25) %>%
  pack_rows("Daily Drinker", 26, 27) %>%
  pack_rows("Census variables", 28, 55) %>%
  pack_rows("Neighbourhood type", 28, 29) %>%
  pack_rows("Individual mean income (CAD/1000)", 30, 31) %>%
  pack_rows("Household mean income (CAD/1000)", 32, 33) %>%
  pack_rows("Individual median income (CAD/1000)", 34, 35) %>%
  pack_rows("Household median income (CAD/1000)", 36, 37) %>%
  pack_rows("Prevalence of low income (%)", 38, 39) %>%
  pack_rows("Commute Walking/Bicycle (%)", 40, 41) %>%
  pack_rows("Labour force participation rate (%)", 42, 43) %>%
  pack_rows("Gov’t transfer payments (%)", 44, 45) %>%
  pack_rows("Unemployment rate (%)", 46, 47) %>%
  pack_rows("Lone parent families (%)", 48, 49) %>%
  pack_rows("Education - No degree (%)", 50, 51) %>%
  pack_rows("Private Dwellings - Owned (%)", 52, 53) %>%
  pack_rows("Private Dwellings - Rented (%)", 54, 55) %>%
  pack_rows("NDVI - Median", 56, 57) %>%
  pack_rows("NDVI - Standard Deviation", 58, 59) %>%
  pack_rows("NDVI - Min", 60, 61) %>%
  pack_rows("NDVI - max", 62, 63)
#> 
#> -- Column specification --------------------------------------------------------
#> cols(
#>   Parameter = col_character(),
#>   `No Diabetes (N=4481)` = col_character(),
#>   `Diabetes (N=497)` = col_character(),
#>   `Total (N=4978)` = col_character()
#> )
```

<table class=" lightable-paper lightable-striped" style='font-family: "Arial Narrow", arial, helvetica, sans-serif; width: auto !important; margin-left: auto; margin-right: auto;'>

<caption>

Table A.3: Baseline characteristics of the study population by diabetes
status.

</caption>

<thead>

<tr>

<th style="text-align:left;">

Parameter

</th>

<th style="text-align:left;">

No Diabetes (N=4481)

</th>

<th style="text-align:left;">

Diabetes (N=497)

</th>

<th style="text-align:left;">

Total (N=4978)

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Total included

</td>

<td style="text-align:left;">

4481 (90.0%)

</td>

<td style="text-align:left;">

497 (10.0%)

</td>

<td style="text-align:left;">

4978

</td>

</tr>

<tr grouplength="2">

<td colspan="4" style="border-bottom: 1px solid;">

<strong>City</strong>

</td>

</tr>

<tr>

<td style="text-align:left; padding-left:  2em;" indentlevel="1">

Hamilton

</td>

<td style="text-align:left;">

2255 (50.3%)

</td>

<td style="text-align:left;">

324 (65.2%)

</td>

<td style="text-align:left;">

2579 (51.8%)

</td>

</tr>

<tr>

<td style="text-align:left; padding-left:  2em;" indentlevel="1">

Vancouver

</td>

<td style="text-align:left;">

2226 (49.7%)

</td>

<td style="text-align:left;">

173 (34.8%)

</td>

<td style="text-align:left;">

2399 (48.2%)

</td>

</tr>

<tr grouplength="24">

<td colspan="4" style="border-bottom: 1px solid;">

<strong>Participant data</strong>

</td>

</tr>

<tr grouplength="2">

<td colspan="4" style="border-bottom: 1px solid;">

<strong>BMI</strong>

</td>

</tr>

<tr>

<td style="text-align:left; padding-left:  2em;" indentlevel="2">

Mean (SD)

</td>

<td style="text-align:left;">

27.3 (5.3)

</td>

<td style="text-align:left;">

32.1 (6.4)

</td>

<td style="text-align:left;">

27.8 (5.6)

</td>

</tr>

<tr>

<td style="text-align:left; padding-left:  2em;" indentlevel="2">

Median (Q1, Q3)

</td>

<td style="text-align:left;">

26.4 (23.8, 29.9)

</td>

<td style="text-align:left;">

30.9 (27.4, 35.8)

</td>

<td style="text-align:left;">

26.8 (24.0, 30.4)

</td>

</tr>

<tr grouplength="2">

<td colspan="4" style="border-bottom: 1px solid;">

<strong>Waist to hip ratio</strong>

</td>

</tr>

<tr>

<td style="text-align:left; padding-left:  2em;" indentlevel="2">

Mean (SD)

</td>

<td style="text-align:left;">

85.2 (9.0)

</td>

<td style="text-align:left;">

93.6 (8.5)

</td>

<td style="text-align:left;">

86.1 (9.3)

</td>

</tr>

<tr>

<td style="text-align:left; padding-left:  2em;" indentlevel="2">

Median (Q1, Q3)

</td>

<td style="text-align:left;">

85.2 (78.6, 91.6)

</td>

<td style="text-align:left;">

94.2 (87.8, 99.6)

</td>

<td style="text-align:left;">

86.0 (79.3, 92.6)

</td>

</tr>

<tr grouplength="2">

<td colspan="4" style="border-bottom: 1px solid;">

<strong>Obesity (WHR)</strong>

</td>

</tr>

<tr>

<td style="text-align:left; padding-left:  2em;" indentlevel="2">

No

</td>

<td style="text-align:left;">

2696 (60.2%)

</td>

<td style="text-align:left;">

112 (22.5%)

</td>

<td style="text-align:left;">

2808 (56.4%)

</td>

</tr>

<tr>

<td style="text-align:left; padding-left:  2em;" indentlevel="2">

Yes

</td>

<td style="text-align:left;">

1785 (39.8%)

</td>

<td style="text-align:left;">

385 (77.5%)

</td>

<td style="text-align:left;">

2170 (43.6%)

</td>

</tr>

<tr grouplength="2">

<td colspan="4" style="border-bottom: 1px solid;">

<strong>Age (years)</strong>

</td>

</tr>

<tr>

<td style="text-align:left; padding-left:  2em;" indentlevel="2">

Mean (SD)

</td>

<td style="text-align:left;">

52.7 (9.3)

</td>

<td style="text-align:left;">

56.9 (8.4)

</td>

<td style="text-align:left;">

53.1 (9.3)

</td>

</tr>

<tr>

<td style="text-align:left; padding-left:  2em;" indentlevel="2">

Median (Q1, Q3)

</td>

<td style="text-align:left;">

53.0 (45.0, 60.0)

</td>

<td style="text-align:left;">

57.0 (51.0, 63.0)

</td>

<td style="text-align:left;">

53.0 (46.0, 61.0)

</td>

</tr>

<tr grouplength="2">

<td colspan="4" style="border-bottom: 1px solid;">

<strong>Sex</strong>

</td>

</tr>

<tr>

<td style="text-align:left; padding-left:  2em;" indentlevel="2">

Male

</td>

<td style="text-align:left;">

2011 (44.9%)

</td>

<td style="text-align:left;">

286 (57.5%)

</td>

<td style="text-align:left;">

2297 (46.1%)

</td>

</tr>

<tr>

<td style="text-align:left; padding-left:  2em;" indentlevel="2">

Female

</td>

<td style="text-align:left;">

2470 (55.1%)

</td>

<td style="text-align:left;">

211 (42.5%)

</td>

<td style="text-align:left;">

2681 (53.9%)

</td>

</tr>

<tr grouplength="6">

<td colspan="4" style="border-bottom: 1px solid;">

<strong>Household Income Range</strong>

</td>

</tr>

<tr>

<td style="text-align:left; padding-left:  2em;" indentlevel="2">

\>90k

</td>

<td style="text-align:left;">

1751 (39.1%)

</td>

<td style="text-align:left;">

127 (25.6%)

</td>

<td style="text-align:left;">

1878 (37.7%)

</td>

</tr>

<tr>

<td style="text-align:left; padding-left:  2em;" indentlevel="2">

45k-65k

</td>

<td style="text-align:left;">

730 (16.3%)

</td>

<td style="text-align:left;">

85 (17.1%)

</td>

<td style="text-align:left;">

815 (16.4%)

</td>

</tr>

<tr>

<td style="text-align:left; padding-left:  2em;" indentlevel="2">

30k-45k

</td>

<td style="text-align:left;">

548 (12.2%)

</td>

<td style="text-align:left;">

96 (19.3%)

</td>

<td style="text-align:left;">

644 (12.9%)

</td>

</tr>

<tr>

<td style="text-align:left; padding-left:  2em;" indentlevel="2">

65k-90k

</td>

<td style="text-align:left;">

973 (21.7%)

</td>

<td style="text-align:left;">

79 (15.9%)

</td>

<td style="text-align:left;">

1052 (21.1%)

</td>

</tr>

<tr>

<td style="text-align:left; padding-left:  2em;" indentlevel="2">

20k-30k

</td>

<td style="text-align:left;">

299 (6.7%)

</td>

<td style="text-align:left;">

65 (13.1%)

</td>

<td style="text-align:left;">

364 (7.3%)

</td>

</tr>

<tr>

<td style="text-align:left; padding-left:  2em;" indentlevel="2">

\<20k

</td>

<td style="text-align:left;">

180 (4.0%)

</td>

<td style="text-align:left;">

45 (9.1%)

</td>

<td style="text-align:left;">

225 (4.5%)

</td>

</tr>

<tr grouplength="2">

<td colspan="4" style="border-bottom: 1px solid;">

<strong>AHEI Score</strong>

</td>

</tr>

<tr>

<td style="text-align:left; padding-left:  2em;" indentlevel="2">

Mean (SD)

</td>

<td style="text-align:left;">

37.7 (10.0)

</td>

<td style="text-align:left;">

35.0 (9.5)

</td>

<td style="text-align:left;">

37.4 (10.0)

</td>

</tr>

<tr>

<td style="text-align:left; padding-left:  2em;" indentlevel="2">

Median (Q1, Q3)

</td>

<td style="text-align:left;">

37.7 (30.7, 44.8)

</td>

<td style="text-align:left;">

34.8 (28.0, 41.4)

</td>

<td style="text-align:left;">

37.4 (30.4, 44.5)

</td>

</tr>

<tr grouplength="2">

<td colspan="4" style="border-bottom: 1px solid;">

<strong>Physical Activity MET Score</strong>

</td>

</tr>

<tr>

<td style="text-align:left; padding-left:  2em;" indentlevel="2">

Mean (SD)

</td>

<td style="text-align:left;">

2.5 (0.6)

</td>

<td style="text-align:left;">

2.4 (0.7)

</td>

<td style="text-align:left;">

2.5 (0.6)

</td>

</tr>

<tr>

<td style="text-align:left; padding-left:  2em;" indentlevel="2">

Median (Q1, Q3)

</td>

<td style="text-align:left;">

3.0 (2.0, 3.0)

</td>

<td style="text-align:left;">

2.0 (2.0, 3.0)

</td>

<td style="text-align:left;">

3.0 (2.0, 3.0)

</td>

</tr>

<tr grouplength="2">

<td colspan="4" style="border-bottom: 1px solid;">

<strong>Ever smoked</strong>

</td>

</tr>

<tr>

<td style="text-align:left; padding-left:  2em;" indentlevel="2">

No

</td>

<td style="text-align:left;">

2048 (55.2%)

</td>

<td style="text-align:left;">

172 (41.2%)

</td>

<td style="text-align:left;">

2220 (53.8%)

</td>

</tr>

<tr>

<td style="text-align:left; padding-left:  2em;" indentlevel="2">

Yes

</td>

<td style="text-align:left;">

1664 (44.8%)

</td>

<td style="text-align:left;">

245 (58.8%)

</td>

<td style="text-align:left;">

1909 (46.2%)

</td>

</tr>

<tr grouplength="2">

<td colspan="4" style="border-bottom: 1px solid;">

<strong>Daily Drinker</strong>

</td>

</tr>

<tr>

<td style="text-align:left; padding-left:  2em;" indentlevel="2">

\>=1 drinks/day

</td>

<td style="text-align:left;">

1140 (30.7%)

</td>

<td style="text-align:left;">

112 (26.9%)

</td>

<td style="text-align:left;">

1252 (30.3%)

</td>

</tr>

<tr>

<td style="text-align:left; padding-left:  2em;" indentlevel="2">

\<1 drink/day

</td>

<td style="text-align:left;">

2572 (69.3%)

</td>

<td style="text-align:left;">

305 (73.1%)

</td>

<td style="text-align:left;">

2877 (69.7%)

</td>

</tr>

<tr grouplength="28">

<td colspan="4" style="border-bottom: 1px solid;">

<strong>Census variables</strong>

</td>

</tr>

<tr grouplength="2">

<td colspan="4" style="border-bottom: 1px solid;">

<strong>Neighbourhood type</strong>

</td>

</tr>

<tr>

<td style="text-align:left; padding-left:  2em;" indentlevel="2">

suburban/rural

</td>

<td style="text-align:left;">

3489 (77.9%)

</td>

<td style="text-align:left;">

398 (80.1%)

</td>

<td style="text-align:left;">

3887 (78.1%)

</td>

</tr>

<tr>

<td style="text-align:left; padding-left:  2em;" indentlevel="2">

urban

</td>

<td style="text-align:left;">

992 (22.1%)

</td>

<td style="text-align:left;">

99 (19.9%)

</td>

<td style="text-align:left;">

1091 (21.9%)

</td>

</tr>

<tr grouplength="2">

<td colspan="4" style="border-bottom: 1px solid;">

<strong>Individual mean income (CAD/1000)</strong>

</td>

</tr>

<tr>

<td style="text-align:left; padding-left:  2em;" indentlevel="2">

Mean (SD)

</td>

<td style="text-align:left;">

38.1 (13.2)

</td>

<td style="text-align:left;">

34.4 (11.5)

</td>

<td style="text-align:left;">

37.7 (13.1)

</td>

</tr>

<tr>

<td style="text-align:left; padding-left:  2em;" indentlevel="2">

Median (Q1, Q3)

</td>

<td style="text-align:left;">

35.0 (30.0, 42.0)

</td>

<td style="text-align:left;">

32.0 (27.0, 39.0)

</td>

<td style="text-align:left;">

34.0 (30.0, 42.0)

</td>

</tr>

<tr grouplength="2">

<td colspan="4" style="border-bottom: 1px solid;">

<strong>Household mean income (CAD/1000)</strong>

</td>

</tr>

<tr>

<td style="text-align:left; padding-left:  2em;" indentlevel="2">

Mean (SD)

</td>

<td style="text-align:left;">

78.8 (27.3)

</td>

<td style="text-align:left;">

71.6 (25.6)

</td>

<td style="text-align:left;">

78.1 (27.2)

</td>

</tr>

<tr>

<td style="text-align:left; padding-left:  2em;" indentlevel="2">

Median (Q1, Q3)

</td>

<td style="text-align:left;">

73.0 (61.0, 91.0)

</td>

<td style="text-align:left;">

66.0 (53.0, 82.0)

</td>

<td style="text-align:left;">

72.0 (60.0, 90.0)

</td>

</tr>

<tr grouplength="2">

<td colspan="4" style="border-bottom: 1px solid;">

<strong>Individual median income (CAD/1000)</strong>

</td>

</tr>

<tr>

<td style="text-align:left; padding-left:  2em;" indentlevel="2">

Mean (SD)

</td>

<td style="text-align:left;">

28.1 (6.0)

</td>

<td style="text-align:left;">

26.6 (5.9)

</td>

<td style="text-align:left;">

27.9 (6.0)

</td>

</tr>

<tr>

<td style="text-align:left; padding-left:  2em;" indentlevel="2">

Median (Q1, Q3)

</td>

<td style="text-align:left;">

28.0 (23.0, 32.0)

</td>

<td style="text-align:left;">

25.0 (22.0, 31.0)

</td>

<td style="text-align:left;">

28.0 (23.0, 32.0)

</td>

</tr>

<tr grouplength="2">

<td colspan="4" style="border-bottom: 1px solid;">

<strong>Household median income (CAD/1000)</strong>

</td>

</tr>

<tr>

<td style="text-align:left; padding-left:  2em;" indentlevel="2">

Mean (SD)

</td>

<td style="text-align:left;">

65.5 (20.1)

</td>

<td style="text-align:left;">

60.9 (19.6)

</td>

<td style="text-align:left;">

65.0 (20.1)

</td>

</tr>

<tr>

<td style="text-align:left; padding-left:  2em;" indentlevel="2">

Median (Q1, Q3)

</td>

<td style="text-align:left;">

62.0 (52.0, 76.0)

</td>

<td style="text-align:left;">

57.0 (46.0, 71.0)

</td>

<td style="text-align:left;">

61.0 (52.0, 75.0)

</td>

</tr>

<tr grouplength="2">

<td colspan="4" style="border-bottom: 1px solid;">

<strong>Prevalence of low income (%)</strong>

</td>

</tr>

<tr>

<td style="text-align:left; padding-left:  2em;" indentlevel="2">

Mean (SD)

</td>

<td style="text-align:left;">

9.6 (6.4)

</td>

<td style="text-align:left;">

11.2 (7.0)

</td>

<td style="text-align:left;">

9.8 (6.5)

</td>

</tr>

<tr>

<td style="text-align:left; padding-left:  2em;" indentlevel="2">

Median (Q1, Q3)

</td>

<td style="text-align:left;">

8.4 (5.0, 13.9)

</td>

<td style="text-align:left;">

9.9 (5.7, 16.4)

</td>

<td style="text-align:left;">

8.5 (5.0, 14.2)

</td>

</tr>

<tr grouplength="2">

<td colspan="4" style="border-bottom: 1px solid;">

<strong>Commute Walking/Bicycle (%)</strong>

</td>

</tr>

<tr>

<td style="text-align:left; padding-left:  2em;" indentlevel="2">

Mean (SD)

</td>

<td style="text-align:left;">

7.6 (6.8)

</td>

<td style="text-align:left;">

6.9 (5.8)

</td>

<td style="text-align:left;">

7.6 (6.7)

</td>

</tr>

<tr>

<td style="text-align:left; padding-left:  2em;" indentlevel="2">

Median (Q1, Q3)

</td>

<td style="text-align:left;">

5.2 (3.1, 9.9)

</td>

<td style="text-align:left;">

5.4 (3.2, 8.6)

</td>

<td style="text-align:left;">

5.3 (3.1, 9.8)

</td>

</tr>

<tr grouplength="2">

<td colspan="4" style="border-bottom: 1px solid;">

<strong>Labour force participation rate (%)</strong>

</td>

</tr>

<tr>

<td style="text-align:left; padding-left:  2em;" indentlevel="2">

Mean (SD)

</td>

<td style="text-align:left;">

66.8 (7.3)

</td>

<td style="text-align:left;">

65.4 (6.9)

</td>

<td style="text-align:left;">

66.7 (7.2)

</td>

</tr>

<tr>

<td style="text-align:left; padding-left:  2em;" indentlevel="2">

Median (Q1, Q3)

</td>

<td style="text-align:left;">

66.8 (61.9, 71.7)

</td>

<td style="text-align:left;">

64.8 (60.8, 69.7)

</td>

<td style="text-align:left;">

66.7 (61.8, 71.4)

</td>

</tr>

<tr grouplength="2">

<td colspan="4" style="border-bottom: 1px solid;">

<strong>Gov’t transfer payments (%)</strong>

</td>

</tr>

<tr>

<td style="text-align:left; padding-left:  2em;" indentlevel="2">

Mean (SD)

</td>

<td style="text-align:left;">

10.0 (5.0)

</td>

<td style="text-align:left;">

12.3 (5.9)

</td>

<td style="text-align:left;">

10.2 (5.1)

</td>

</tr>

<tr>

<td style="text-align:left; padding-left:  2em;" indentlevel="2">

Median (Q1, Q3)

</td>

<td style="text-align:left;">

9.1 (6.0, 12.9)

</td>

<td style="text-align:left;">

11.4 (7.5, 16.0)

</td>

<td style="text-align:left;">

9.3 (6.1, 13.3)

</td>

</tr>

<tr grouplength="2">

<td colspan="4" style="border-bottom: 1px solid;">

<strong>Unemployment rate (%)</strong>

</td>

</tr>

<tr>

<td style="text-align:left; padding-left:  2em;" indentlevel="2">

Mean (SD)

</td>

<td style="text-align:left;">

5.6 (2.2)

</td>

<td style="text-align:left;">

6.3 (2.5)

</td>

<td style="text-align:left;">

5.6 (2.2)

</td>

</tr>

<tr>

<td style="text-align:left; padding-left:  2em;" indentlevel="2">

Median (Q1, Q3)

</td>

<td style="text-align:left;">

5.5 (4.0, 6.8)

</td>

<td style="text-align:left;">

6.0 (4.8, 7.6)

</td>

<td style="text-align:left;">

5.6 (4.1, 6.9)

</td>

</tr>

<tr grouplength="2">

<td colspan="4" style="border-bottom: 1px solid;">

<strong>Lone parent families (%)</strong>

</td>

</tr>

<tr>

<td style="text-align:left; padding-left:  2em;" indentlevel="2">

Mean (SD)

</td>

<td style="text-align:left;">

15.0 (5.8)

</td>

<td style="text-align:left;">

17.3 (6.9)

</td>

<td style="text-align:left;">

15.2 (6.0)

</td>

</tr>

<tr>

<td style="text-align:left; padding-left:  2em;" indentlevel="2">

Median (Q1, Q3)

</td>

<td style="text-align:left;">

14.4 (11.5, 18.0)

</td>

<td style="text-align:left;">

16.1 (12.7, 21.2)

</td>

<td style="text-align:left;">

14.5 (11.6, 18.3)

</td>

</tr>

<tr grouplength="2">

<td colspan="4" style="border-bottom: 1px solid;">

<strong>Education - No degree (%)</strong>

</td>

</tr>

<tr>

<td style="text-align:left; padding-left:  2em;" indentlevel="2">

Mean (SD)

</td>

<td style="text-align:left;">

19.7 (9.0)

</td>

<td style="text-align:left;">

23.8 (10.0)

</td>

<td style="text-align:left;">

20.1 (9.2)

</td>

</tr>

<tr>

<td style="text-align:left; padding-left:  2em;" indentlevel="2">

Median (Q1, Q3)

</td>

<td style="text-align:left;">

17.7 (13.1, 25.2)

</td>

<td style="text-align:left;">

20.9 (16.0, 31.6)

</td>

<td style="text-align:left;">

17.9 (13.3, 26.0)

</td>

</tr>

<tr grouplength="2">

<td colspan="4" style="border-bottom: 1px solid;">

<strong>Private Dwellings - Owned (%)</strong>

</td>

</tr>

<tr>

<td style="text-align:left; padding-left:  2em;" indentlevel="2">

Mean (SD)

</td>

<td style="text-align:left;">

74.0 (16.3)

</td>

<td style="text-align:left;">

73.2 (15.8)

</td>

<td style="text-align:left;">

73.9 (16.3)

</td>

</tr>

<tr>

<td style="text-align:left; padding-left:  2em;" indentlevel="2">

Median (Q1, Q3)

</td>

<td style="text-align:left;">

76.5 (62.6, 87.4)

</td>

<td style="text-align:left;">

74.6 (63.0, 85.7)

</td>

<td style="text-align:left;">

76.3 (62.6, 87.3)

</td>

</tr>

<tr grouplength="2">

<td colspan="4" style="border-bottom: 1px solid;">

<strong>Private Dwellings - Rented (%)</strong>

</td>

</tr>

<tr>

<td style="text-align:left; padding-left:  2em;" indentlevel="2">

Mean (SD)

</td>

<td style="text-align:left;">

25.7 (16.0)

</td>

<td style="text-align:left;">

26.4 (15.3)

</td>

<td style="text-align:left;">

25.8 (15.9)

</td>

</tr>

<tr>

<td style="text-align:left; padding-left:  2em;" indentlevel="2">

Median (Q1, Q3)

</td>

<td style="text-align:left;">

23.2 (12.5, 37.2)

</td>

<td style="text-align:left;">

25.3 (14.0, 36.9)

</td>

<td style="text-align:left;">

23.5 (12.6, 37.2)

</td>

</tr>

<tr grouplength="2">

<td colspan="4" style="border-bottom: 1px solid;">

<strong>NDVI - Median</strong>

</td>

</tr>

<tr>

<td style="text-align:left; padding-left:  2em;" indentlevel="1">

Mean (SD)

</td>

<td style="text-align:left;">

0.344 (0.087)

</td>

<td style="text-align:left;">

0.318 (0.081)

</td>

<td style="text-align:left;">

0.341 (0.087)

</td>

</tr>

<tr>

<td style="text-align:left; padding-left:  2em;" indentlevel="1">

Median (Q1, Q3)

</td>

<td style="text-align:left;">

0.337 (0.294, 0.377)

</td>

<td style="text-align:left;">

0.315 (0.270, 0.359)

</td>

<td style="text-align:left;">

0.335 (0.291, 0.375)

</td>

</tr>

<tr grouplength="2">

<td colspan="4" style="border-bottom: 1px solid;">

<strong>NDVI - Standard Deviation</strong>

</td>

</tr>

<tr>

<td style="text-align:left; padding-left:  2em;" indentlevel="1">

Mean (SD)

</td>

<td style="text-align:left;">

0.098 (0.024)

</td>

<td style="text-align:left;">

0.096 (0.023)

</td>

<td style="text-align:left;">

0.098 (0.024)

</td>

</tr>

<tr>

<td style="text-align:left; padding-left:  2em;" indentlevel="1">

Median (Q1, Q3)

</td>

<td style="text-align:left;">

0.095 (0.082, 0.111)

</td>

<td style="text-align:left;">

0.092 (0.081, 0.106)

</td>

<td style="text-align:left;">

0.095 (0.081, 0.111)

</td>

</tr>

<tr grouplength="2">

<td colspan="4" style="border-bottom: 1px solid;">

<strong>NDVI - Min</strong>

</td>

</tr>

<tr>

<td style="text-align:left; padding-left:  2em;" indentlevel="1">

Mean (SD)

</td>

<td style="text-align:left;">

0.193 (0.095)

</td>

<td style="text-align:left;">

0.170 (0.082)

</td>

<td style="text-align:left;">

0.191 (0.094)

</td>

</tr>

<tr>

<td style="text-align:left; padding-left:  2em;" indentlevel="1">

Median (Q1, Q3)

</td>

<td style="text-align:left;">

0.180 (0.125, 0.243)

</td>

<td style="text-align:left;">

0.153 (0.110, 0.227)

</td>

<td style="text-align:left;">

0.177 (0.124, 0.241)

</td>

</tr>

<tr grouplength="2">

<td colspan="4" style="border-bottom: 1px solid;">

<strong>NDVI - max</strong>

</td>

</tr>

<tr>

<td style="text-align:left; padding-left:  2em;" indentlevel="1">

Mean (SD)

</td>

<td style="text-align:left;">

0.510 (0.087)

</td>

<td style="text-align:left;">

0.479 (0.089)

</td>

<td style="text-align:left;">

0.507 (0.087)

</td>

</tr>

<tr>

<td style="text-align:left; padding-left:  2em;" indentlevel="1">

Median (Q1, Q3)

</td>

<td style="text-align:left;">

0.503 (0.457, 0.552)

</td>

<td style="text-align:left;">

0.474 (0.425, 0.527)

</td>

<td style="text-align:left;">

0.501 (0.452, 0.550)

</td>

</tr>

</tbody>

</table>

# About

### Package contributors:

Brinkmann, Sebastian Tobias (Package creator and author)  
e-mail: <sebastian.brinkmann@fau.de>  
Große, Tim (Contributor)

### Thesis authors:

Walker, Blake Byron (1\*)  
Brinkmann, Sebastian Tobias (1)  
Große, Tim (1)  

1: Community Health Environments and Social Terrains (CHEST) Lab,
Institut für Geographie, Friedrich-Alexander-Universität
Erlangen-Nürnberg, Wetterkreuz 15, 91052 Erlangen, Germany  
\*corresponding author  

# Bibliography

  - work in progress -
