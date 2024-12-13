test
================

## Test

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(readr)
library(tidyr)
library(ggplot2)
library(janitor)
```

    ## 
    ## Attaching package: 'janitor'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     chisq.test, fisher.test

``` r
library(sf)
```

    ## Linking to GEOS 3.8.0, GDAL 3.0.4, PROJ 6.3.1

``` r
library(kableExtra)
```

    ## 
    ## Attaching package: 'kableExtra'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     group_rows

``` r
library(mapview)
library(rnaturalearth)
source("../R/col_types_wells.R")
message("install rnaturalearthhires")
```

    ## install rnaturalearthhires

``` r
install.packages("rnaturalearthhires", repos = "http://packages.ropensci.org", type = "source")
```

    ## Installing package into '/usr/local/lib/R/site-library'
    ## (as 'lib' is unspecified)

``` r
message("done install rnaturalearthhires")
```

    ## done install rnaturalearthhires

``` r
bc_yellow <- "#e3a82b"
bc_blue <- "#234075"

ggplot2::theme_set(theme_light()) 
options(ggplot2.discrete.fill  = function() scale_fill_viridis_d()) # these scales are colorbliend friendly and start with "cooperators blue"
options(ggplot2.continuous.fill  = function() scale_fill_viridis_c())
options(ggplot2.discrete.colour = function() scale_color_viridis_d())
options(ggplot2.continuous.colour = function() scale_color_viridis_c())

today <- as.Date(Sys.time() , tz = "America/Vancouver")
start <- today - 14 
```

``` r
current_well <- read_csv("../data/current_well.csv", col_types = col_types_wells )# from R/coltypes_well 
```

    ## Warning: One or more parsing issues, see `problems()` for details

``` r
list_of_date_added <- read_csv("../data/list_of_date_added.csv", col_types =  readr::cols(well_tag_number = col_double(), date_added = col_date()))

wells <- current_well %>% 
  left_join(list_of_date_added) %>%
  janitor::clean_names() 
```

    ## Joining, by = "well_tag_number"

``` r
wells %>% count(date_added) %>%
  ggplot()+
  geom_line(aes(x = date_added, y = n)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)) +
  labs(
    title = "New wells per day"
  )
```

![](test_github_report_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
recent_wells <- wells  %>% 
  filter(date_added <= today, date_added >= start )



recent_wells %>% 
  tail(20)
```

    ## # A tibble: 20 × 114
    ##    well_tag_number identification_plate_number well_identifica… well_status_code
    ##              <dbl>                       <dbl> <chr>            <chr>           
    ##  1          131731                          NA <NA>             NEW             
    ##  2          131732                       40852 <NA>             NEW             
    ##  3          131733                       41855 <NA>             NEW             
    ##  4          131734                       60116 SIDE OF CASING   NEW             
    ##  5          131735                       71413 On well casing   NEW             
    ##  6          131736                       71318 On well casing   NEW             
    ##  7          131737                       63044 casing           NEW             
    ##  8          131738                       63011 casing           NEW             
    ##  9          131739                       10448 casing           ALTERATION      
    ## 10          131740                       67992 casing           NEW             
    ## 11          131741                       67981 casing           NEW             
    ## 12          131742                       70303 casing           NEW             
    ## 13          131743                       67993 casing           NEW             
    ## 14          131744                       20965 <NA>             NEW             
    ## 15          131745                       67985 casing           NEW             
    ## 16          131746                       67967 casing           NEW             
    ## 17          131747                       70316 casing           NEW             
    ## 18          131748                       70321 casing           NEW             
    ## 19          131749                       70324 casing           NEW             
    ## 20          131750                       70326 casing           NEW             
    ## # … with 110 more variables: well_class_code <chr>, well_subclass <chr>,
    ## #   licenced_status_code <chr>, intended_water_use_code <chr>,
    ## #   observation_well_number <chr>, obs_well_status_code <chr>,
    ## #   water_supply_system_name <chr>, water_supply_system_well_name <chr>,
    ## #   street_address <chr>, city <chr>, legal_lot <chr>, legal_plan <chr>,
    ## #   legal_district_lot <chr>, legal_block <chr>, legal_section <chr>,
    ## #   legal_township <chr>, legal_range <chr>, land_district_code <chr>, …

``` r
countries <- ne_states(country= "Canada", returnclass = "sf")

wells.sf <- recent_wells %>% 
  tail(1000) %>% 
  filter(!is.na(latitude_decdeg) & !is.na(longitude_decdeg)) %>%
    st_as_sf(coords = c( "longitude_decdeg", "latitude_decdeg" ), crs = 4326, remove = FALSE) 

ggplot() +
  geom_sf(data = wells.sf, alpha = 0.2, color = "blue") +
  geom_sf(data = countries %>% filter(name == "British Columbia"), fill  = NA) +
  coord_sf(expand = FALSE, crs = 3153) + 
  theme_minimal()
```

![](test_github_report_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->
