Zip Code Cleaning and Imputation
================
Quinton Neville
November 20, 2018

Pulling Exhaustive list of NYC Zips
===================================

``` r
nyc.official.zip <- read_csv("./data/Demographic_Statistics_By_Zip_Code.csv") %>%
  janitor::clean_names() %>% 
  rename(official_zip = jurisdiction_name) %>%
  select(official_zip)


official.zip <- paste0(nyc.official.zip$official_zip, collapse = "|")
  
#https://data.cityofnewyork.us/City-Government/Demographic-Statistics-By-Zip-Code/kku6-nxdu
```

Imputing Zip Codes
==================

``` r
dog.bite.df <- read_csv("./data/DOHMH_Dog_Bite_Data.csv") %>%
  janitor::clean_names() %>%
  mutate(
    zip_code = zip_code %>% 
               ifelse(str_length(.) != 5, NA, .) %>% 
               str_replace(., "O", "0") %>% 
               ifelse(str_detect(., "^[02-9]"), NA, .),
    zip_match = str_detect(zip_code, official.zip),
    zip_code  = ifelse(zip_match == FALSE, NA, zip_code)
  ) %>%
  left_join(.,
            na.omit(.) %>% 
            left_join(., count(., borough), by = "borough") %>% 
            group_by(borough, zip_code) %>%
            summarize(proportion = n()/unique(n)) %>% 
            rename(zip_list = zip_code) %>%
            nest(zip_list, proportion) %>% rename(zip_nest = data))

#Missing Zips
dog.bite.df %>% summarize(missing_zip = sum(is.na(zip_code))/n())
```

    ## # A tibble: 1 x 1
    ##   missing_zip
    ##         <dbl>
    ## 1       0.248

``` r
#Missing Zips by borough
dog.bite.df %>% group_by(borough) %>% summarize(missing_zip = sum(is.na(zip_code))/n())
```

    ## # A tibble: 6 x 2
    ##   borough       missing_zip
    ##   <chr>               <dbl>
    ## 1 Bronx               0.243
    ## 2 Brooklyn            0.131
    ## 3 Manhattan           0.318
    ## 4 Other               0.921
    ## 5 Queens              0.185
    ## 6 Staten Island       0.276

``` r
#Set Seed
set.seed(4)

#Imputing Zip Code
dog.bite.df <- dog.bite.df %>%
  mutate(
    zip_sample = map_chr(.x = zip_nest, ~sample(.x$zip_list, 1, prob = .x$proportion)),
    zip_code_imputed = zip_code %>% ifelse(is.na(.), zip_sample, .)
  ) %>% select(-c(zip_nest, zip_sample))

#View Data
dog.bite.df %>% select(borough, zip_code, zip_code_imputed)
```

    ## # A tibble: 8,707 x 3
    ##    borough       zip_code zip_code_imputed
    ##    <chr>         <chr>    <chr>           
    ##  1 Staten Island <NA>     10301           
    ##  2 Brooklyn      11217    11217           
    ##  3 Brooklyn      <NA>     11215           
    ##  4 Brooklyn      11236    11236           
    ##  5 Brooklyn      11204    11204           
    ##  6 Brooklyn      <NA>     11215           
    ##  7 Brooklyn      <NA>     11238           
    ##  8 Brooklyn      11220    11220           
    ##  9 Brooklyn      11229    11229           
    ## 10 Brooklyn      11216    11216           
    ## # ... with 8,697 more rows

``` r
#Num Distinct Zips
dog.bite.df %>% distinct(., zip_code_imputed) %>% nrow()
```

    ## [1] 183
