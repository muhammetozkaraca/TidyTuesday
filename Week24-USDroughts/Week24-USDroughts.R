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

drought <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-14/drought.csv')
drought_fips <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-14/drought-fips.csv')
data(fips_codes)
data(county.fips)
us_states <- map_data("county")

county_geodata <- county.fips %>%
  separate(polyname, c("region","subregion"), ",") %>%
  merge(us_states, by = c("region", "subregion")) %>%
  mutate(county_code = str_sub(fips, 2, 5))

data <- drought_fips %>%
  mutate(state_code = str_sub(FIPS, 1, 2),
         county_code = str_sub(FIPS, 3, 5)) %>%
  merge(fips_codes, by = c("state_code", "county_code")) %>%
  dplyr::mutate(year = lubridate::year(date), 
                month = lubridate::month(date), 
                day = lubridate::day(date)) %>%
  group_by(year) %>%
  mutate(annual_average = mean (DSCI)) %>%
  select(-c(date, FIPS))

colnames(data)[6] <- "region"
data$region <- str_to_lower(data$region)

merged <- data %>%
  left_join(county_geodata, by = c("region", "county_code"))


data_2021 <- merged %>%
  filter(year == 2021)

ggplot(data_2021, aes(long, lat, group = group)) +
  geom_polygon()

merged %>%
  filter(year == 2021) %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon() +
  theme_void() +
  
  
  
  
  
  
  
  labs(title = "2021 Drought", 
       subtitle = "<span style = 'font-size:12pt'>The Tuskegee Airmen were a group of African American military pilots who fought during World War II. This map visualizes the distribution of their hometown states. 
       In total, this data represents <span style='color:#B22222'>994</span> pilots' hometowns. Please note that the original dataset includes <span style='color:#B22222'>12</span> more pilots 
       for whom the data about their hometown was not included in this visualisation, as it was missing.",
       caption = "#TidyTuesday Challenge | Muhammet Ozkaraca") +


