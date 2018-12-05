Final Report
================
all
12/4/2018

Motivation
----------

Pit bulls have a historically poor reputation as an aggressive breed. They're most commonly used in dog fighting, and are often not permitted as pets in many leasing communities. Supporters of the breed have championed these dogs as sweet and loving in nature, and in recent years have been pushing to change the negative stigma. Based on the occurrence of dog bites, can we see whether pit bulls are more aggressive than other breeds, and whether this has changed over time?

Initial questions
-----------------

Several questions arose for us:

-   Are there more bites given by pit bulls than other breeds?
-   Does this number change by borough? Over time?
-   Are there more pitbulls in certain neighborhoods?
-   Do pit bulls tend to be spayed/neutered more or less often than other breeds?
-   How does the number of dog bites by breed compare to the total number of registered dogs in a certain time period?
-   Does season / weather affect the number of dog bites?

Data
----

### Data sources

All data was taken from [NYC Open Data](https://opendata.cityofnewyork.us). We examine two datasets to answer our research questions:

**Dog Licensing data**: Active Dog Licenses during 2016.

All dog owners residing in NYC are required by law to license their dogs. The data is sourced from the [DOHMH Dog Licensing System](https://a816-healthpsi.nyc.gov/DogLicense), where owners can apply for and renew dog licenses. Each record represents a unique dog license that was active during the year, but not necessarily a unique record per dog, since a license that is renewed during the year results in a separate record of an active license period. Each record stands as a unique license period for the dog over the course of the yearlong time frame.

Variables provided: RowNumber, AnimalName, AnimalGender, AnimalBirthMonth, BreedName, Borough, ZipCode, CommunityDistrict, CensusTract2010, NTA, CityCouncilDistrict, CongressionalDistrict, StateSenatorialDistrict, LicenseIssuedDate, LicenseExpiredDate

**Dog Bite data** NYC Reported Dog Bites (2015 - 2018)

Information reported assists the Health Department to determine if the biting dog is healthy ten days after the person was bitten in order to avoid having the person bitten receive unnecessary rabies shots. Data is collected from reports received online, mail, fax or by phone to 311 or NYC DOHMH Animal Bite Unit. Each record represents a single dog bite incident. Information on breed, age, gender and spayed or neutered status have not been verified by DOHMH and is listed only as reported to DOHMH.

Variables provided: UniqueID, DateOfBite, Species, Breed, Age, Gender, SpayNeuter, Borough, ZipCode

### Data cleaning

The biggest hurdle to overcome in cleaning the data was identifying the borough (Bronx, Brooklyn, Manhattan, Queens, Staten Island). There were many miscellaneous (e.g. Jersey City) or narrow (e.g. Astoria, which is part of Queens) entries, and identifying them required a bit of labor.

In the dog bite data, we had the additional challenge of verifying validity of existing zip codes and imputing zip codes where they were missing or invalid. Through some exploration, we found that about 25% of the data (~2,000 reports) had a missing or invalid zip code. We imputed on these entries by assuming that the listed borough was correct and (1) getting a full list of true zip codes by borough from the data found in the `zipcode` library; (2) matching the verifying whether the zip codes given were legitimate by cross referencing said data; and (3) imputing a zip code for missing entries based on frequencies found in the non-missing dog bite data.

The licensing data required us to clean the borough names. We did this by (1) manually obtaining a list of clearly mis-classified borough entries and recoding them properly; (2)

``` r
# list of Queens neighborhood strings found in license/dog bite datasets
queens <- c("astoria", "elmhurst", "flushing", "jackson heights", "kew gardens", "woodside", "briarwood", "corona", "forest hills", "ozone park", "richmond hill", "rockaway park", "arverne", "bayside", "belle harbor", "cambria heights", "college point", "east elmhurst", "glen oaks", "glendale", "jackson hgts", "long island city", "maspeth", "middle vilg", "middle village", "oakland gardens", "quens", "ridgewood", "so richmond", "south richmond hil", "woodside ny.", "fresh meadows", 'queens village')

# list of boroughs found in license data that are not actually in NYC
not_nyc <- c("jersey city", "albany", "bronxville", "floral park", "hoboken", "kissimmee florida", "lynbrook", "middletown", "san francisco", "santa monica", "wappingers falls, ny", "west palm beach", "the villages")

# list of staten island strings
staten_island <- c("staten is", "staten island, ny", "staten island")

# unknown / vague borough
unknown_borough <- c("new york", "new york city", "nyc", "ny", "b", "potomac", NA)


# get list of real zipcodes for each borough using the 'zipcode' library
data(zipcode) 
zipcode = zipcode %>%
  mutate(city = tolower(city),
         borough = ifelse(city %in% c('new york', 'manhattan'), 'Manhattan', NA), # recode borough
         borough = ifelse(city == 'bronx', 'Bronx', borough),
         borough = ifelse(city == 'brooklyn', 'Brooklyn', borough),
         borough = ifelse(city %in% c('queens', queens), 'Queens', borough),
         borough = ifelse(city %in% c('staten island', staten_island), 'Staten Island', borough)) %>%
  filter(borough %in% c('Manhattan', 'Bronx', 'Brooklyn', 'Queens', 'Staten Island') & state == 'NY')

# create nested data frame of zip codes by borough to impute on missing dog bite zip codes
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
  left_join(., # left join the nested zip codes on the dog bite data by borough
            na.omit(.) %>% 
            left_join(., count(., borough), by = "borough") %>% 
            group_by(borough, zip_code) %>%
            summarize(proportion = n()/unique(n)) %>% 
            rename(zip_list = zip_code) %>%
            nest(zip_list, proportion) %>% 
            rename(zip_nest = data),
            by = 'borough') %>% 
  mutate(
    zip_sample = map_chr(.x = zip_nest, ~sample(.x$zip_list, 1, prob = .x$proportion)), # impute zip codes where missing based on frequency with which they appear in the non-missing data
    zip_code_imputed = zip_code %>% ifelse(is.na(.), zip_sample, .)
  ) %>% 
  select(-c(zip_nest, zip_sample)) # final data frame of dog bites
```

**Licensing data**

``` r
# function to capitalize first letter of words
simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1, 1)), substring(s, 2),
          sep = "", collapse = " ")
}

# clean license data
license <- read_csv("./data/NYC_Dog_Licensing_Dataset.csv") %>%
  mutate(Borough = tolower(Borough),
         LicenseIssuedDate = as.Date(LicenseIssuedDate, "%m/%d/%Y"),
         LicenseExpiredDate = as.Date(LicenseExpiredDate, "%m/%d/%Y"),
         issued_year = year(LicenseIssuedDate),
         expired_year = year(LicenseExpiredDate),
         cleaned_borough = ifelse(Borough %in% queens, "queens", Borough), # clean borough names where obvious
         cleaned_borough = ifelse(cleaned_borough %in% not_nyc, "not nyc", cleaned_borough),
         cleaned_borough = ifelse(cleaned_borough %in% staten_island, "Staten Island", cleaned_borough),
         cleaned_borough = ifelse(cleaned_borough %in% unknown_borough, "Other", cleaned_borough),
         zip = as.character(ZipCode)) %>% # recast zip code as character
  left_join(., zipcode, by = 'zip') %>% # left join the zipcode data from 'zipcode' library
  rowwise() %>%
  mutate(borough = ifelse(is.na(borough), simpleCap(cleaned_borough), simpleCap(borough))) %>% # assign borough from original license data if 
  rename(final_borough = borough) %>% # rename variable
  select(RowNumber, BreedName, ZipCode, Borough, cleaned_borough, final_borough, issued_year, expired_year) %>%
  filter(tolower(cleaned_borough) == tolower(final_borough)) %>%
  mutate(BreedName = tolower(BreedName),
         pit_bull = str_detect(BreedName, 'pit bull')) %>%
  janitor::clean_names() %>%
  filter(cleaned_borough != 'not nyc')
```

Exploratory analysis
--------------------

Assumptions made in this analysis: - each reported bite is made by a different dog - no dog appears in the licensing dataset more than once - all dogs that bite are licensed - dogs bite only in their borough

``` r
# dog bite data from only 2016 to match dog licensing data
dg_2016 = dog_bite %>%
  filter(date_of_bite > '2016-01-01' & date_of_bite < '2016-12-31')
```

We were interested to know how the raw counts of dog bites compared to the number of registered dogs; if there are more pit bull bites, does that mean that pit bulls are more aggressive and likely to bite than other breeds, or is it simply that there are more pit bulls in the city?

``` r
# number of dog bites (pitbull vs. other) by boro
bite_counts = dg_2016 %>%
  group_by(borough, pit_bull) %>% # group by borough and whether the dog was a pit bull or not
  count %>% # get total counts
  ungroup %>%
  spread(key = pit_bull, value = n)  %>%
  `colnames<-`(c('boro', 'not_pitbull_bites', 'pitbull_bites')) %>%
  mutate(total_bites = `not_pitbull_bites` + `pitbull_bites`)

# number of registered dogs (pitbull vs. other) by boro
license_counts = license %>%
  group_by(final_borough, pit_bull) %>%
  count %>%
  spread(key = pit_bull, value = n) %>%
  dplyr::select(1:3) %>%
  `colnames<-`(c('boro', 'not_pitbull_lic', 'pitbull_lic')) %>%
  mutate(total_lic = `not_pitbull_lic` + `pitbull_lic`)


master = dplyr::inner_join(bite_counts, license_counts, by = 'boro') %>%
  filter(boro != 'Other')
```

If we assume that no dog bites twice, and all dogs that bite are licensed, then across all boroughs, 2.09% of licensed dogs bite. By borough, it breaks down as follows:

``` r
# print table of dog bites / licenses by borough
master %>%
  mutate('Percent' = round((total_bites / total_lic) * 100, 2)) %>%
  dplyr::select(boro, total_lic, total_bites, Percent) %>%
  arrange(-total_lic) %>%
  `colnames<-`(c('Borough', 'No. Licenses', 'No. bites', 'Percent')) %>%
  knitr::kable()
```

| Borough       |  No. Licenses|  No. bites|  Percent|
|:--------------|-------------:|----------:|--------:|
| Manhattan     |         42507|        530|     1.25|
| Brooklyn      |         30271|        598|     1.98|
| Queens        |         25109|        664|     2.64|
| Bronx         |         12585|        486|     3.86|
| Staten Island |         11146|        264|     2.37|

A higher percentage of licensed dogs bite in the Bronx than any other NYC borough; Manhattan has the lowest percentage of total dog bites. However, Manhattan has the most licensed dogs by quite a bit, whereas the Bronx has the second fewest number of registered dogs. Queens has the highest number of reported bites (664 total).

We can break this analysis down by dog breed; pit bull or not. Across all years, 32.57% of all bites are given by pitbulls; in 2016, this number was 35.74%. This is a rather high percentage for a single breed.

The figures below show the number of registered dogs and number of bites by borough, and classified by breed type. Only a small percentage of registered dogs are pitbulls (A), but upwards of 30% of all reported bites are given by pit bulls (B).

``` r
# number of licenses in each borough in 2016 by breed type
lic_bar = license %>%
  filter(final_borough != 'Other') %>% # remove unknown borough
  ggplot(aes(x = final_borough, fill = pit_bull)) +
  geom_bar() +  # bar plot
  viridis::scale_fill_viridis( # color scheme
    name = 'Dog breed',
    labels = c('Other', 'Pit Bull'),
    discrete = T
  ) + 
  theme(legend.position = 'bottom') + 
  labs(title = '(A) Dog licenses (2016)',
       x = '',
       y = 'Number of licenses')

# number of dog bites in each borough by breed type
bite_bar_2016 = dg_2016 %>%
  filter(borough != 'Other') %>%
  ggplot(aes(x = borough, fill = pit_bull)) +
  geom_bar() + 
  viridis::scale_fill_viridis(
    name = 'Dog breed',
    labels = c('Other', 'Pit Bull'),
    discrete = T
  ) + 
  labs(title = '(B) Dog bites (2016)',
       x = '',
       y = 'Number of bites') +
  theme(legend.position = 'right')


# print figures
lic_bar + theme(legend.position = 'none',
                axis.text.x = element_text(angle = 45, hjust = 1)) + bite_bar_2016 + theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

<img src="Final_report_files/figure-markdown_github/unnamed-chunk-7-1.png" width="90%" style="display: block; margin: auto;" />

Are pitbulls spayed/neutered more often than other breeds?

``` r
dog_bite %>%
  group_by(gender, spay_neuter, pit_bull) %>%
  count() %>% # counts of spay/neuter status by sex and breed type
  group_by(pit_bull) %>%
  mutate(gender = recode(gender, 'F' = 'Female', 'M' = 'Male', 'U' = 'Unknown'), # recode gender variable
         spay_neuter = ifelse(spay_neuter == TRUE, "Yes", "No")) %>% # recode spay/neuter status
  ggplot(aes(x = spay_neuter, y = n, fill = pit_bull)) + 
  geom_bar(stat = "identity",  position = "stack") + 
  #geom_text(aes(label = n), position = position_stack(vjust = 0.5)) + 
  #scale_fill_manual(values = c('#FFF8DC','#9370DB')) + 
  labs(y = "Number of Bites",
       x = "Spayed/Neutered Status") +
  #theme_hc() + 
  facet_grid(~ gender) + 
  viridis::scale_fill_viridis(
    name = 'Dog breed',
    labels = c('Other', 'Pit Bull'),
    discrete = T
  )
```

<img src="Final_report_files/figure-markdown_github/unnamed-chunk-8-1.png" width="90%" style="display: block; margin: auto;" />

With respect to the temporal dynamics of pit bull bites versus non-pitbulls, from the beggining of 2015 through the end of 2017, we observed a moderate overall increase in the mean number of bites per month by non-pitbulls. However, for pit bulls, the trend in number of bites per month is more stable, portraying a slight decrease in the mean number of bites in time.

``` r
 time.overall.df <- dog_bite %>%
  mutate(date = ymd(date_of_bite)) %>% 
  mutate_at(vars(date), funs(year, month, day)) %>%
  mutate(date_numeric = lubridate::decimal_date(date)) %>%
  select(-date) %>% 
  mutate(borough = as.factor(borough)) %>% 
  group_by(year, month) %>%
  summarise(
    pitbull    = sum(pit_bull),
    no_pitbull = sum(!(pit_bull))
  ) %>% ungroup() %>%
  gather(key = pit_bull, value = num_bites, pitbull:no_pitbull) %>%
  mutate(
    pit_bull = ifelse(pit_bull == "pitbull", "Pit Bull", "Other") %>% as.factor(),
#    pit_bull = fct_relevel(pit_bull, "Yes"),
    date_numeric = year + (month - 1)/12
  )


time.overall.plot <- time.overall.df %>%
 ggplot(aes(x = date_numeric, y = num_bites, colour = pit_bull)) + 
  geom_point(alpha = 0.3, size = 2) + 
  geom_line(size = 1) + 
  geom_smooth(aes(colour = pit_bull), size = 1.5, alpha = 0.5, se = F, method = "lm") + 
    theme(legend.position = "bottom",
        axis.text.y = element_text(color = "black", 
                                   size = 10,  hjust = 1), 
        axis.text.x = element_text(angle = 45, 
                                   hjust = 1, size = 10)) +
  labs(
    x = "Time",
    y = "Number of Bites per Month",
    title = "Number of Bites Over Time"
  ) + 
    viridis::scale_colour_viridis(
    name = "Dog Breed", 
    discrete = TRUE
   ) +
  xlim(c(2015, 2018)) 

time.overall.plot
```

<img src="Final_report_files/figure-markdown_github/unnamed-chunk-9-1.png" width="90%" style="display: block; margin: auto;" />

Additionally, we observed an oscillating fluctation in seasonal effects, with overall dog bites rising during the summer months and falling during the winter months.

Next, we considered the borough level effects on the temporal patterns of dog bites.

``` r
#Create Bites Per Month Data Frame, by Borough
time.boro.df <- dog_bite %>%
  mutate(date = ymd(date_of_bite)) %>% 
  mutate_at(vars(date), funs(year, month, day)) %>%
  select(-date) %>% 
  mutate(borough = as.factor(borough)) %>% group_by(year, month, borough) %>%
  summarise(
    pitbull    = sum(pit_bull),
    no_pitbull = sum(!(pit_bull))
  ) %>% ungroup() %>%
  gather(key = pit_bull, value = num_bites, pitbull:no_pitbull) %>%
  mutate(
    pit_bull = ifelse(pit_bull == "pitbull", "Pit Bull", "Other") %>% as.factor(),
    date_numeric = year + (month - 1)/12
  )

#Lineplot of Bites per Month by Borough
time.boro.plot <- time.boro.df %>%
  filter(borough != "Other") %>%
  ggplot(aes(x = date_numeric, y = num_bites, colour = pit_bull)) + 
  geom_line(size = 1, alpha = 0.6) + 
  geom_smooth(aes(colour = pit_bull), size = 1.5, alpha = 0.3, se = F, method = "lm") + 
  theme(legend.position = "bottom",
        axis.text.y = element_text(color = "black", 
                                   size = 10,  hjust = 1), 
        axis.text.x = element_text(angle = 45, 
                                   hjust = 1, size = 10)) +
  labs(
    x = "Time",
    y = "Number of Bites per Month",
    title = "Number of Bites by Borough in Time"
  ) + 
  viridis::scale_colour_viridis(
    name = "Dog Breed", 
    discrete = TRUE
   ) +
  xlim(c(2015, 2018)) +
  facet_grid(~borough)

time.boro.plot
```

<img src="Final_report_files/figure-markdown_github/unnamed-chunk-10-1.png" width="90%" style="display: block; margin: auto;" />

In Manhattan, we observed the largest mean difference in pit bull vs. non-pit bull bites per month but no significant change in the moderately increasing trend between the two over time. Similarly, in Staten Island a smaller mean difference was observed but with a nearly identical decreasing trend. Brooklyn and Queens, however, demonstrated a widening gap over time, with the pit bull bites decreasing and non-pit bull bites increasing significantly. Lastly, the Bronx evinced a differential increase in pit bull bites over time while non-pit bull bites decreased, converging to a similar mean number of bites per month by the end of 2017.

Additional analysis
-------------------

Discussion
----------