library(tidyverse)
library(ggtext)
library(gganimate)
library(glue)
library(showtext)
library(countrycode)
library(geofacet)
library(tidycensus)
library(lubridate)
library(sf)
library(gifski) # to save the animated plot. 

drought_fips <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-14/drought-fips.csv')
data("fips_codes")

counties <- tigris::counties() %>% 
  select(state_id = STATEFP , county_id = GEOID , county_name = NAMELSAD) %>%
  rename(fips = county_id) %>%
  filter(!state_id %in% c("02" , "15" , "72")) %>%
  st_simplify(preserveTopology = TRUE, dTolerance = 1000) 

data <- 
  drought_fips %>%
  janitor::clean_names() %>%
  # formatting
  rename(state_abb   = "state") %>%
  mutate(state_code  = substr(fips, start = 1, stop = 2),
         county_code = substr(fips, start = 3, stop = 5)) %>%
  left_join(fips_codes, by = c("state_code", "county_code")) %>%
  dplyr::mutate(year = lubridate::year(date), 
                month = lubridate::month(date), 
                day = lubridate::day(date)) %>%
  group_by(county_code, year, month) %>%
  mutate(monthly_average = mean (dsci)) %>%
  filter(!state_code %in% c("02", "15", "72")) %>%
  left_join(counties, by = c("state_code" = "state_id", "county" = "county_name")) %>%
  select(state_abb, dsci, date, year, month, day, state_name, county, monthly_average, geometry)

font_add_google("Fascinate Inline", family = "title")
font_add_google("Libre Caslon Display", family = "subtitle")
font_add_google("Libre Caslon Display", family = "caption")
font_add_google("Patrick Hand", family = "year")


font_add('fa-reg', 'fonts/Font Awesome 6 Free-Regular-400.otf')
font_add('fa-brands', 'fonts/Font Awesome 6 Brands-Regular-400.otf')
font_add('fa-solid', 'fonts/Font Awesome 6 Free-Solid-900.otf')

showtext_auto()

# to convert the date from d/m/y format to m/y format, 
# we need to complete the following 2 lines of code
desired_date_format <- stamp( "01-2020", orders = "my") 
data$month_year <- desired_date_format(data$date)


plot <- data %>%
  filter(year>2019) %>%
  ggplot() +
  geom_sf(aes(fill = monthly_average, geometry = geometry), color = "white", size = 0.1) +
  paletteer::scale_fill_paletteer_c("ggthemes::Orange",
                                    name = "Level of \nDrought",
                                    limits = c(0, 500), breaks = c(0, 250, 500)) +
  ggthemes::theme_map() +
  transition_manual(month_year) +
  shadow_mark() +
  labs(title = "Drought Level in the US Counties (2020-2022)",
       subtitle = "Climate change is becoming more apparent day by day. Accordingly, this animation illustrates monthly level of drought in the US counties <span style = 'font-size:30pt;color:#E69F00;'>since 2020.</span> <br>
       The tone of the color for each county is based on the Drought Severity and Coverage Index. While <span style = 'font-size:30pt;color:#E69F00;'>0</span> indicates none of the area is <br> abnormally dry or in drought, <span style = 'font-size:30pt;color:#E69F00;'>500</span> means that all of the area is in exceptional drought.",
       caption = "Source: National Integrated Drought Information System | Plot: <span style='font-family:fa-brands'>&#xf09b;</span> muhammetozkaraca <span style='font-family:fa-brands'>&#xf099;</span> muhammetozkrca | #TidyTuesday-Week 24") + 
  theme(
    plot.background = element_rect(fill = "white",colour = NA),
    panel.background = element_rect(fill = 'white', colour = 'white'),
    panel.border = element_blank(),
    strip.text.x = element_text(family = "year", size = 19),
    strip.background = element_rect(fill = 'white', colour = 'white'),
    plot.title = element_text(hjust = 0.5, family = "title", size = 41),
    plot.subtitle = element_markdown(hjust = 0.5, size = 29), # note that to color numbers differently, I used element_markdown not element_text function
    plot.caption = element_markdown(hjust = 0.5, family = "caption", size = 17),
    legend.position = c(0.75, 0.05),
    legend.direction = "horizontal",
    legend.key.width = unit(4.5, "line"),
    legend.key.height = unit(1, 'cm'), 
    legend.key.size = unit(1, 'cm'), 
    legend.title = element_text(size=14), 
    legend.text = element_text(size=10))

plot %>%
  animate(fps = 20 , nframe = 300, 
          height = 2000 , width = 2340,
          start_pause = 1, end_pause = 1,
          renderer = gifski_renderer(here::here("Week24-USDroughts/us_droughts_county_level.gif")))





  






