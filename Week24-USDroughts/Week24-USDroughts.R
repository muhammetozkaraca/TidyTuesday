library(tidyverse)
library(ggtext)
library(glue)
library(showtext)
library(countrycode)
library(geofacet)
library(tidycensus)
library(lubridate)
library(maps)
library(usmap)

drought_fips <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-14/drought-fips.csv')
counties_geodata <- map_data("county")
data("fips_codes")


fips_codes$county <- gsub("County", "", as.character(fips_codes$county))
fips_codes$county <- str_to_lower(fips_codes$county)
fips_codes$state_name <- str_to_lower(fips_codes$state_name)



data <- drought_fips %>%
  janitor::clean_names() %>% 
  mutate(state_code = str_sub(fips, 1, 2),
         county_code = str_sub(fips, 3, 5)) %>%
  dplyr::mutate(year = lubridate::year(date), 
                month = lubridate::month(date), 
                day = lubridate::day(date)) %>%
  group_by(county_code, year, month) %>%
  mutate(monthly_average = mean (dsci)) %>%
  filter(!state_code %in% c("02", "15", "72")) %>%
  left_join(fips_codes, by = c("state_code" = "state_code", "county_code" = "county_code")) %>%
  merge(counties_geodata, by = c("state_name" = "region", "county" =  "subregion"))

data %>%
  filter(year == 2000, month == 1, day == 4) %>%
  ggplot(aes(x = long, y = lat, group = group, fill = dsci)) +
  geom_polygon()









