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
    ##  1          126200                       65594 <NA>             NEW             
    ##  2          126201                       45586 Surface casing … NEW             
    ##  3          126202                       16073 <NA>             NEW             
    ##  4          126203                          NA <NA>             CLOSURE         
    ##  5          126204                          NA <NA>             NEW             
    ##  6          126205                          NA <NA>             CLOSURE         
    ##  7          126206                          NA <NA>             CLOSURE         
    ##  8          126207                          NA <NA>             CLOSURE         
    ##  9          126209                       68283 casing           NEW             
    ## 10          126210                       42347 Well stickup     NEW             
    ## 11          126211                       42349 On well head     NEW             
    ## 12          126212                          NA <NA>             CLOSURE         
    ## 13          126213                       68284 casing           NEW             
    ## 14          126214                       53714 <NA>             NEW             
    ## 15          126215                       67045 On well casing   NEW             
    ## 16          126216                          NA <NA>             CLOSURE         
    ## 17          126217                       67032 On well casing   NEW             
    ## 18          126218                       66443 On well casing   NEW             
    ## 19          126219                          NA <NA>             CLOSURE         
    ## 20          126220                          NA <NA>             CLOSURE         
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
