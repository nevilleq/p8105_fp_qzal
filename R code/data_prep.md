Data cleaning/prep
================
Alyssa Vanderbeek
11/27/2018

Dog Bite data
-------------

``` r
queens <- c("astoria", "elmhurst", "flushing", "jackson heights", "kew gardens", "woodside", "briarwood", "corona", "forest hills", "ozone park", "richmond hill", "rockaway park", "arverne", "bayside", "belle harbor", "cambria heights", "college point", "east elmhurst", "glen oaks", "glendale", "jackson hgts", "long island city", "maspeth", "middle vilg", "middle village", "oakland gardens", "quens", "ridgewood", "so richmond", "south richmond hil", "woodside ny.", "fresh meadows", 'queens village')

not_nyc <- c("jersey city", "albany", "bronxville", "floral park", "hoboken", "kissimmee florida", "lynbrook", "middletown", "san francisco", "santa monica", "wappingers falls, ny", "west palm beach", "the villages")

staten_island <- c("staten is", "staten island, ny", "staten island")

unknown_borough <- c("new york", "new york city", "nyc", "ny", "b", "potomac", NA)


# get list of real zipcodes for each borough
data(zipcode) 
zipcode = zipcode %>%
  mutate(city = tolower(city),
         borough = ifelse(city %in% c('new york', 'manhattan'), 'Manhattan', NA),
         borough = ifelse(city == 'bronx', 'Bronx', borough),
         borough = ifelse(city == 'brooklyn', 'Brooklyn', borough),
         borough = ifelse(city %in% c('queens', queens), 'Queens', borough),
         borough = ifelse(city %in% c('staten island', staten_island), 'Staten Island', borough)) %>%
  filter(borough %in% c('Manhattan', 'Bronx', 'Brooklyn', 'Queens', 'Staten Island') & state == 'NY')

zip_list = zipcode %>%
  select(borough, zip) %>%
  nest(-borough)
```

``` r
# list of official NYC zip codes 
official.zip = paste(zipcode$zip, collapse = '|')

# dog bite data
dog_bite = read_csv('./data/DOHMH_Dog_Bite_Data.csv',
                    col_type = cols(UniqueID = col_integer(),
                        DateOfBite = col_date(format = '%B %d %Y'), # set as date variable
                        Species = col_character(),
                        Breed = col_character(),
                        Age = col_number(),
                        Gender = col_character(),
                        SpayNeuter = col_logical(),
                        Borough = col_character(),
                        ZipCode = col_character()
                      )) %>% # read data
  janitor::clean_names() %>%
  mutate(pit_bull = str_detect(tolower(breed), 'pit bull|pitbull|pit-bull')) %>% # logical column for whether dog was a pitbull breed or not
  mutate(
    zip_code = zip_code %>% 
               ifelse(str_length(.) != 5, NA, .) %>%  # set zip codes without length 5 equal to NA
               str_replace(., "O", "0") %>% # replace O's with zeros
               ifelse(str_detect(., "^[02-9]"), NA, .), # ?
    zip_match = str_detect(zip_code, official.zip), # is the given zip code valid?
    zip_code  = ifelse(zip_match == FALSE, NA, zip_code)
  ) %>%
  left_join(.,
            na.omit(.) %>% 
            left_join(., count(., borough), by = "borough") %>% 
            group_by(borough, zip_code) %>%
            summarize(proportion = n()/unique(n)) %>% 
            rename(zip_list = zip_code) %>%
            nest(zip_list, proportion) %>% 
            rename(zip_nest = data),
            by = 'borough') %>% 
  mutate(
    zip_sample = map_chr(.x = zip_nest, ~sample(.x$zip_list, 1, prob = .x$proportion)),
    zip_code_imputed = zip_code %>% ifelse(is.na(.), zip_sample, .)
  ) %>% 
  select(-c(zip_nest, zip_sample))

write.csv(dog_bite, './data/dog_bite_12.4.csv', row.names = F)
```

Licensing data
--------------

``` r
# function to capitalize first letter of words
simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1, 1)), substring(s, 2),
          sep = "", collapse = " ")
}

license_df <- read_csv("./data/NYC_Dog_Licensing_Dataset.csv") %>%
  mutate(Borough = tolower(Borough),
         LicenseIssuedDate = as.Date(LicenseIssuedDate, "%m/%d/%Y"),
         LicenseExpiredDate = as.Date(LicenseExpiredDate, "%m/%d/%Y"),
         issued_year = year(LicenseIssuedDate),
         expired_year = year(LicenseExpiredDate),
         cleaned_borough = Borough)
```

    ## Parsed with column specification:
    ## cols(
    ##   RowNumber = col_integer(),
    ##   AnimalName = col_character(),
    ##   AnimalGender = col_character(),
    ##   AnimalBirthMonth = col_character(),
    ##   BreedName = col_character(),
    ##   Borough = col_character(),
    ##   ZipCode = col_integer(),
    ##   CommunityDistrict = col_integer(),
    ##   CensusTract2010 = col_character(),
    ##   NTA = col_character(),
    ##   CityCouncilDistrict = col_integer(),
    ##   CongressionalDistrict = col_integer(),
    ##   StateSenatorialDistrict = col_integer(),
    ##   LicenseIssuedDate = col_character(),
    ##   LicenseExpiredDate = col_character()
    ## )

``` r
unknown_borough_df <- license_df %>%
  filter(cleaned_borough == "unknown") %>%
  select(RowNumber, ZipCode, cleaned_borough)

zip_code_borough <- read_csv("./zelos/zip_codes_borough.csv") # change this to use the data that Quinton downloaded from OpenSource
```

    ## Parsed with column specification:
    ## cols(
    ##   ZipCode = col_integer(),
    ##   supposed_borough = col_character()
    ## )

``` r
license_df <- license_df %>%
  mutate(cleaned_borough = ifelse(cleaned_borough %in% queens, "queens", cleaned_borough),
         cleaned_borough = ifelse(cleaned_borough %in% not_nyc, "not nyc", cleaned_borough),
         cleaned_borough = ifelse(cleaned_borough %in% staten_island, "Staten Island", cleaned_borough),
         cleaned_borough = ifelse(cleaned_borough %in% unknown_borough, "Other", cleaned_borough),
         zip = as.character(ZipCode))

license_df <- left_join(license_df, zipcode, by = 'zip')

license_df <- license_df %>%
  rowwise() %>%
  mutate(borough = ifelse(is.na(borough), simpleCap(cleaned_borough), borough)) %>%
  rename(final_borough = borough)

counts_df <- license_df %>%
  select(RowNumber, BreedName, ZipCode, Borough, cleaned_borough, final_borough, issued_year, expired_year) %>%
  ungroup

agree_ids <- counts_df$RowNumber[which(tolower(counts_df$cleaned_borough) == tolower(counts_df$final_borough))]

agreed_df <- counts_df %>%
  filter(RowNumber %in% agree_ids) %>%
  mutate(BreedName = tolower(BreedName),
         pit_bull = str_detect(BreedName, 'pit bull')) %>%
  janitor::clean_names() %>%
  filter(cleaned_borough != 'not nyc')

# only 312 out of 121949 licenses are now a problem
problematic_df <- counts_df %>%
  filter(!RowNumber %in% agree_ids)

write.csv(agreed_df, './data/cleaned_license_data.csv', row.names = F)
```