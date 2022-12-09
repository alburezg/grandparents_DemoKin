How many grandparents are there in the world?
================
Dec 04 2022

  - [1. Installation](#1-installation)
  - [2. Get data](#2-get-data)
  - [3. Approximate the number of grandparentes in a
    population](#3-approximate-the-number-of-grandparentes-in-a-population)
  - [Compare to simulations](#compare-to-simulations)
  - [References](#references)

We will use matrix kinship models in a time-variant framework (Caswell
and Song 2021) to compute the expected number of grandparents and
grandchildren in a range of countries and the related kin dependencies.

The code runs in R, preferably in RStudio.

The intuition here is that we can approximate the number of grandparents
in a population in year `y` from a number of known quantities: a) the
age distribution of grandparents of Focal, b) the age distribution of
grandchildren for people alive in year `y`, and c) population size by
age in year `y`.

<img src="DemoKin-Logo.png" align="right" width="200" />

# 1\. Installation

Install the [development version](https://github.com/IvanWilli/DemoKin)
of DemoKin from GitHub (could take \~1 minute). We made changes to the
`DemoKin` package recently. If you had already installed the package,
please uninstall it and and install it again.

``` r
# remove.packages("DemoKin")
# install.packages("devtools")
devtools::install_github("IvanWilli/DemoKin", build_vignettes = TRUE)
```

Load packages:

``` r
library(DemoKin)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(countrycode)
library(knitr)
```

Define a function to get necessary data from the UNWPP

``` r
get_UNWPP_inputs <- function(countries, my_startyr, my_endyr, variant = "Median"){
  
  
  print("Getting API ready...")
  # Get data from UN using API
  
  base_url <- 'https://population.un.org/dataportalapi/api/v1'
  
  # First, identify which indicator codes we want to use
  
  target <- paste0(base_url,'/indicators/?format=csv')
  codes <- read.csv(target, sep='|', skip=1) 
  
  qx_code <- codes$Id[codes$ShortName == "qx1"]
  asfr_code <- codes$Id[codes$ShortName == "ASFR1"]
  pop_code <- codes$Id[codes$ShortName == "PopByAge1AndSex"]
  
  # Get location codes
  
  target <- paste0(base_url, '/locations?sort=id&format=csv')
  df_locations <- read.csv(target, sep='|', skip=1)
  
  # find the codes for countries
  
  my_location <- 
    df_locations %>% 
    filter( Name %in% countries) %>% 
    pull(Id) %>% 
    paste(collapse = ",")
  
  # Get px values
  
  print(paste0("Getting mortality data for ", paste(countries, collapse = ", ")))
  
  my_indicator <- qx_code
  my_location  <- my_location
  
  target <- paste0(base_url,
                   '/data/indicators/',my_indicator,
                   '/locations/',my_location,
                   '/start/',my_startyr,
                   '/end/',my_endyr,
                   '/?format=csv')
  
  px <- 
    read.csv(target, sep='|', skip=1) %>% 
    filter(Variant %in% variant) %>% 
    filter(Sex == "Female") %>% 
    mutate(px = 1- Value) %>% 
    select(Location, Time = TimeLabel, age = AgeStart, px)
  
  # ASFR
  
  print(paste0("Getting fertility data for ", paste(countries, collapse = ", ")))
  
  my_indicator <- asfr_code
  
  target <- paste0(base_url,
                   '/data/indicators/',my_indicator,
                   '/locations/',my_location,
                   '/start/',my_startyr,
                   '/end/',my_endyr,
                   '/?format=csv')
  
  asfr <- 
    read.csv(target, sep='|', skip=1) %>% 
    filter(Variant %in% variant) %>% 
    select(Location, Time = TimeLabel, age = AgeStart, ASFR = Value)
  
  data <- 
    px %>% 
    left_join(asfr, by = c("Location", "Time", "age")) %>% 
    mutate(ASFR = replace(ASFR,is.na(ASFR),0)) 
  
  data
}

# To get UN population
get_unwpp_pop <- function(countries,  my_startyr = 2022, my_endyr = 2022){
  base_url <- 'https://population.un.org/dataportalapi/api/v1'
  
  # First, identify which indicator codes we want to use
  
  target <- paste0(base_url,'/indicators/?format=csv')
  codes <- read.csv(target, sep='|', skip=1) 
  
  pop_code <- codes$Id[codes$ShortName == "PopByAge1AndSex"]
  
  # Get location codes
  
  target <- paste0(base_url, '/locations?sort=id&format=csv')
  df_locations <- read.csv(target, sep='|', skip=1)
  
  # find the codes for countries
  iso3 <- countrycode(countries, origin = "country.name", destination = "iso3c")
  
  locs <- 
    df_locations %>% 
    filter(Iso3 %in% iso3) %>% 
    pull(Id) 
  
  my_location <- paste(locs, collapse = ",")
  
  print(paste0("Getting pop data for ", paste(countries, collapse = ", ")))
  
  
  # Avoid overwhelming UN APi
  if(length(countries) <= 20){
    
    my_indicator <- pop_code
    my_location  <- my_location
    
    target <- paste0(base_url,
                     '/data/indicators/',my_indicator,
                     '/locations/',my_location,
                     '/start/',my_startyr,
                     '/end/',my_endyr,
                     '/?format=csv')
    
    pop <- 
      read.csv(target, sep='|', skip=1) %>% 
      filter(Variant == "Median") %>% 
      select(iso3 = Iso3, country = Location, year = TimeLabel, age = AgeStart, sex = Sex, value = Value)
    
  } else{
    print("Many countries, I'll process in batch")
    
    my_indicator <- pop_code
    
    times <- floor(length(locs)/10)
    sp_vec <- rep(1:10, times)
    extras <- length(locs) - length(sp_vec)
    if(extras > 0) sp_vec <- c(sp_vec, 1:extras)
    
    my_location_l  <- split(locs, sp_vec)
    
    pop <- 
      lapply(1:length(my_location_l), function(n, my_location_l){
        
        print(paste0("Processing batch ", n, "/", length(my_location_l) ))
        
        loc_n <- paste(my_location_l[[n]], collapse = ",")
        
        target <- paste0(base_url,
                         '/data/indicators/',my_indicator,
                         '/locations/', loc_n,
                         '/start/',my_startyr,
                         '/end/',my_endyr,
                         '/?format=csv')
        
        pop <- read.csv(target, sep='|', skip=1)
        
        Sys.sleep(1)
        pop
      }, my_location_l) %>% 
      bind_rows() %>% 
      filter(Variant == "Median") %>% 
      select(iso3 = Iso3, country = Location, year = TimeLabel, age = AgeStart, sex = Sex, value = Value)
    
  }
  
  return(pop) 
  
}
```

# 2\. Get data

Let’s get the numbers of China, Guatemala, and Germany:

``` r
# pick countries
countries <- c("China", "Guatemala", "Germany")
# countries <- c("Guatemala")

output_period <- c(2000, 2020)

# Year range

my_startyr   <- 1950
my_endyr     <- 2022

data <- get_UNWPP_inputs(
  countries = countries
  , my_startyr = my_startyr
  , my_endyr = my_endyr
  )
```

    ## [1] "Getting API ready..."
    ## [1] "Getting mortality data for China, Guatemala, Germany"
    ## [1] "Getting fertility data for China, Guatemala, Germany"

``` r
# Get UN population
# World population in 2022
pop <- 
  # get_unwpp_pop(countries, my_startyr = 2022, my_endyr = 2022) %>% 
    get_unwpp_pop(countries, my_startyr = min(output_period), my_endyr = max(output_period)) %>% 
  filter(sex == "Both sexes") %>%
  select(iso3, year, age, pop_un = value)
```

    ## [1] "Getting pop data for China, Guatemala, Germany"

Run kinship models for 2022 period:

``` r
period_kin_l <- lapply(split(data, list(data$Location)), function(X){
      print(unique(X$Location))
    U <-
      X %>%
      select(Time, age, px) %>%
      pivot_wider(names_from = Time, values_from = px) %>%
      select(-age) %>% 
      as.matrix()
    f <- X %>%
      select(Time, age, ASFR) %>%
      mutate(ASFR = ASFR/1000) %>% 
      pivot_wider(names_from = Time, values_from = ASFR) %>%
      select(-age) %>% 
      as.matrix()
    k <- kin(U, f, time_invariant = FALSE, output_kin = c("gm","gd"), output_period = output_period)
    k$kin_summary$Location <- 
      k$kin_full$Location <- unique(X$Location)
    k
})
```

    ## [1] "China"

    ## Stable assumption was made for calculating pi on each year because no input data.

    ## Warning: replacing previous import 'lifecycle::last_warnings' by
    ## 'rlang::last_warnings' when loading 'hms'

    ## Assuming stable population before 1950.

    ## [1] "Germany"

    ## Stable assumption was made for calculating pi on each year because no input data.

    ## Assuming stable population before 1950.

    ## [1] "Guatemala"

    ## Stable assumption was made for calculating pi on each year because no input data.
    ## Assuming stable population before 1950.

``` r
period_kin_full <- 
  lapply(period_kin_l, `[[`, 'kin_full') %>% 
  bind_rows()
```

# 3\. Approximate the number of grandparentes in a population

Once we have all the data points that we need, we proceed to implement
the estimation. Let
![A(x)](https://latex.codecogs.com/png.latex?A%28x%29 "A(x)") be the sum
of all individuals who are ‘unique’ grandparents to people aged
![x](https://latex.codecogs.com/png.latex?x "x"):

  
![&#10;A(x) = p(x) \\times \\sum\_{i=0}^{100}\\left\[a(x,i) \\times
\\frac{n(i,x)}{\\sum n(i, :)}
\\right\]&#10;](https://latex.codecogs.com/png.latex?%0AA%28x%29%20%3D%20p%28x%29%20%5Ctimes%20%5Csum_%7Bi%3D0%7D%5E%7B100%7D%5Cleft%5Ba%28x%2Ci%29%20%5Ctimes%20%5Cfrac%7Bn%28i%2Cx%29%7D%7B%5Csum%20n%28i%2C%20%3A%29%7D%20%5Cright%5D%0A
"
A(x) = p(x) \\times \\sum_{i=0}^{100}\\left[a(x,i) \\times \\frac{n(i,x)}{\\sum n(i, :)} \\right]
")  
<!-- A(x) = p(x) \times \sum_{i=0}^{100}a(x,i) \times \underbrace{\frac{n(i,x)}{\sum n(i, :)}}_{\substack{\text{share of grandkids}\\ \text{aged i}}}   -->

where

  - ![p(x)](https://latex.codecogs.com/png.latex?p%28x%29 "p(x)") is the
    number of individuals aged
    ![x](https://latex.codecogs.com/png.latex?x "x") inthe population
  - ![a(x,i)](https://latex.codecogs.com/png.latex?a%28x%2Ci%29
    "a(x,i)") is the average number of grandparents aged
    ![i](https://latex.codecogs.com/png.latex?i "i") for a Focal aged
    ![x](https://latex.codecogs.com/png.latex?x "x")
  - ![n(i,x)](https://latex.codecogs.com/png.latex?n%28i%2Cx%29
    "n(i,x)") is the average number of grandchildren aged
    ![x](https://latex.codecogs.com/png.latex?x "x") for a Focal aged
    ![i](https://latex.codecogs.com/png.latex?i "i")
  - ![n(i,:)](https://latex.codecogs.com/png.latex?n%28i%2C%3A%29
    "n(i,:)") is the age distribution of grandchildren for a Focal aged
    ![i](https://latex.codecogs.com/png.latex?i "i")

The total number of grandparents in the population is
![\\sum\_{x=0}^{100}A(x)](https://latex.codecogs.com/png.latex?%5Csum_%7Bx%3D0%7D%5E%7B100%7DA%28x%29
"\\sum_{x=0}^{100}A(x)").

Let’s consider a simplified example:

1.  Focal is 10yo
2.  Focal has 0.4 grandmothers aged 85 and 0.1 grandmothers aged 90
3.  20% of an 85yo Focal is 10yo and 10% of an 90yo Focal is 10yo  
4.  In the ‘real’ population, there are 1,000 women aged 10
5.  The ‘real’ number of grandmothers of 10yo Focals in the population
    is approximately: 1000 \* (0.4 \* 0.2 + 0.1 \* 0.1) = 90
    grandmothers

We start by considering the age distribution of grandparents for Focal.
Since DemoKin currently assumes a matrilinear female population, we
multiply the numbers by 4 to obtain values foe all grandparents (i.e.,
we use ‘GKP factors’):

``` r
gp <- 
  period_kin_full %>% 
  filter(kin == "gm") %>% 
  select(Location, year, age_focal, age_gp = age_kin, gm = living) %>% 
  # these are just maternal grandmothers. multiply by 4?
  mutate(gp = gm*4) %>% 
  select(-gm)

# Let's see the age distribution of grandparents for a 15 yo Focal

gp %>% 
  filter(Location == "China", year == 2000, age_focal %in% c(0, 15, 30)) %>% 
  mutate(age_focal = as.character(age_focal)) %>% 
  ggplot(aes(x = age_gp, y = gp, colour = age_focal)) +
  geom_line() +
  theme_bw()
```

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

Next, we get the distribution of grandchildren for Focals in the same
population and year(s). In addition to this, we also compute the share
of grandchildren aged x for a Focal aged z (so that it sums to 1 across
all age\_gd values):

``` r
gd <- 
  period_kin_full %>% 
  filter(kin == "gd") %>% 
  select(Location, year, age_focal, age_gd = age_kin, gd = living) %>% 
  group_by(Location, year, age_focal) %>% 
  mutate(
    share_gd = gd / sum(gd)
    , share_gd = ifelse(is.nan(share_gd), 0, share_gd)
    ) %>% 
  ungroup()

# What % of a grandparent's aged 90 are 60yo?

gd %>% 
  filter(Location == "China", year == 2000, age_focal %in% c(60, 75, 90)) %>% 
  pivot_longer(gd:share_gd) %>% 
  mutate(age_focal = as.character(age_focal)) %>% 
  ggplot(aes(x = age_gd, y = value, colour = age_focal)) +
  geom_line() +
  facet_grid(.~ name) +
  theme_bw()
```

![](README_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

Implement the algorithm by left joining rather than looping for
efficiency:

``` r
gpd <- 
  left_join(
    gp, gd
    , by = c("Location", "year", "age_gp" = "age_focal", "age_focal" = "age_gd")
    ) %>% 
  mutate(iso3 = countrycode(Location, origin = "country.name", destination = "iso3c")) %>% 
  select(-Location)

# What % of a 20yo Focal's grandparents are x y old?

gpd %>% 
  filter(iso3 == "CHN", year == 2000, age_focal %in% c(0, 15, 60)) %>% 
  mutate(age_focal = as.character(age_focal)) %>%
  pivot_longer(gp:share_gd) %>% 
  ggplot(aes(x = age_gp, y = value, colour = age_focal)) +
  geom_line() +
  facet_grid(.~name) +
  theme_bw()
```

![](README_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
# Get total number of grandparentes

gp_num_age <- 
  gpd %>% 
  group_by(iso3, year, age_focal) %>% 
  summarise(gp_sum = sum(gp*share_gd)) %>% 
  ungroup() %>% 
  # gp_sum is the expected number of 'unique' grandparents for a Focal aged x
  # accounting for the fact some grandparents are shared by multiple grandchildren
  left_join(
    pop %>% select(iso3, year, age, pop_un)
    , by = c("iso3", "year", "age_focal" = "age")
  ) %>%
  mutate(gp_num = pop_un * gp_sum)
```

    ## `summarise()` has grouped output by 'iso3', 'year'. You can override using the
    ## `.groups` argument.

``` r
# 
# gp_num_age %>% 
#   filter(iso3 == "CHN", year == 2000) %>% 
#   pull(gp_num) %>% 
#   plot()

gp_num <- 
  gp_num_age %>% 
  group_by(iso3, year) %>% 
  summarise(gp_tot = sum(gp_num)) %>% 
  ungroup()
```

    ## `summarise()` has grouped output by 'iso3'. You can override using the `.groups`
    ## argument.

``` r
# Results

gp_num %>% 
  mutate(gp_tot = gp_tot/1e6 * 4) %>% 
  kable()
```

| iso3 | year |    gp\_tot |
| :--- | ---: | ---------: |
| CHN  | 2000 | 383.536428 |
| CHN  | 2020 | 404.950568 |
| DEU  | 2000 |  19.046675 |
| DEU  | 2020 |  18.250674 |
| GTM  | 2000 |   5.006927 |
| GTM  | 2020 |   6.424005 |

# Compare to simulations

``` r
sim <- read.csv("Data/grandparents_by_country_age.csv", stringsAsFactors = F)

iso3_countries <- countrycode(countries, origin = "country.name", destination = "iso3c")

comp <- 
  gp_num_age %>% 
  select(iso3, year, age = age_focal, model = gp_num) %>% 
  left_join(
      sim %>% select(iso3, year, age, sim = number_grandparents)
      , by = c("iso3", "year", "age")
  ) %>% 
  pivot_longer(model:sim)


comp %>% 
  ggplot(aes(x = age, y = value, colour = name)) +
  geom_line() +
  facet_wrap(year~iso3, scale = "free") +
  theme_bw()
```

    ## Warning: Removed 2 row(s) containing missing values (geom_path).

![](README_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

The two estimates don’t match but that would be fine since they are not
the same. DemoKin produces the number of unique grandparents by
granchild’s age, which is why the values for the red line are higher at
younger ages. The simulation gives the actual distribution of
grandparents over grandparental age. So, the x axis in both plts is
different (for the model, x is the age of the child and for the
simulation, x is grandparental age).

Even if we sum it up over all ages, the estimates we get are not very
close, with DemoKin giving much lower numbers:

``` r
comp %>% 
  group_by(iso3, year, name) %>% 
  summarise(value = sum(value, na.rm = T)) %>% 
  ungroup() %>% 
  pivot_wider() %>% 
  mutate(diff = model-sim) %>% 
  kable()
```

    ## `summarise()` has grouped output by 'iso3', 'year'. You can override using the
    ## `.groups` argument.

| iso3 | year |     model |       sim |        diff |
| :--- | ---: | --------: | --------: | ----------: |
| CHN  | 2000 |  95884107 | 223053295 | \-127169188 |
| CHN  | 2020 | 101237642 | 333311632 | \-232073990 |
| DEU  | 2000 |   4761669 |  17710786 |  \-12949117 |
| DEU  | 2020 |   4562668 |  18473634 |  \-13910966 |
| GTM  | 2000 |   1251732 |   1628973 |    \-377241 |
| GTM  | 2020 |   1606001 |   2739971 |   \-1133970 |

# References

<div id="refs" class="references">

<div id="ref-caswell_formal_2021">

Caswell, Hal, and Xi Song. 2021. “The Formal Demography of Kinship. III.
Kinship Dynamics with Time-Varying Demographic Rates.” *Demographic
Research* 45: 517–46.

</div>

</div>
