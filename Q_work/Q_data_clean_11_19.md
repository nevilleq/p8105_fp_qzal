Zip Code Cleaning and Imputation
================
Quinton Neville
November 20, 2018

Imputing Zip Codes
==================

``` r
dog.bite.df <- read_csv("./data/DOHMH_Dog_Bite_Data.csv") %>%
  janitor::clean_names() %>%
  mutate(
    zip_code = zip_code %>% 
               ifelse(str_length(.) != 5, NA, .) %>% 
               str_replace(., "O", "0") %>% 
               ifelse(str_detect(., "^[02-9]"), NA, .)
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
    ## 1       0.222

``` r
#Missing Zips by borough
dog.bite.df %>% group_by(borough) %>% summarize(missing_zip = sum(is.na(zip_code))/n())
```

    ## # A tibble: 6 x 2
    ##   borough       missing_zip
    ##   <chr>               <dbl>
    ## 1 Bronx               0.227
    ## 2 Brooklyn            0.114
    ## 3 Manhattan           0.308
    ## 4 Other               0.649
    ## 5 Queens              0.174
    ## 6 Staten Island       0.231

``` r
#Imputing Zip Code
dog.bite.df <- dog.bite.df %>%
  mutate(
    zip_sample = map_chr(.x = zip_nest, ~sample(.x$zip_list, 1, prob = .x$proportion)),
    zip_code_imputed = zip_code %>% ifelse(is.na(.), zip_sample, .)
  ) 
```
