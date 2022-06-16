library(tidyverse)
library(geojsonio)
library(rgdal)
library(broom)
library(rgeos)
library(paletteer)
library(showtext)

# Download the Hexagones boundaries at geojson format here: https://team.carto.com/u/andrew/tables/andrew.us_states_hexgrid/public/map.

spdf <- geojson_read("/Users/muhammetozkaraca/Desktop/TidyTuesday/Week24-USDroughts/us_states_hexgrid.geojson",  what = "sp")

spdf@data = spdf@data %>%
  mutate(google_name = gsub(" \\(United States\\)", "", google_name))

spdf@data = spdf@data %>% 
  mutate(google_name = gsub(" \\(United States\\)", "", google_name))
spdf_fortified <- tidy(spdf, region = "google_name")

# Calculate the centroid of each hexagon to add the label:
centers <- cbind.data.frame(data.frame(gCentroid(spdf, byid=TRUE), id=spdf@data$iso3166_2))

ggplot() +
  geom_polygon(data = spdf_fortified, aes( x = long, y = lat, group = group), fill="skyblue", color="white") +
  geom_text(data=centers, aes(x=x, y=y, label=id)) +
  theme_void() +
  coord_map()


drought_fips <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-14/drought-fips.csv')

data <- drought_fips %>%
  mutate(state_code = str_sub(FIPS, 1, 2),
         county_code = str_sub(FIPS, 3, 5)) %>%
  merge(fips_codes, by = c("state_code", "county_code")) %>%
  dplyr::mutate(year = lubridate::year(date), 
                month = lubridate::month(date), 
                day = lubridate::day(date)) %>%
  group_by(state_name, year) %>%
  mutate(annual_average = mean (DSCI)) %>%
  select(state_name, state, annual_average, year) %>%
  left_join(spdf_fortified, by=c("state_name"="id")) %>%
  distinct()

font_add_google("Fascinate Inline", family = "title")
font_add_google("Cormorant Garamond", family = "subtitle")
font_add_google("Libre Caslon Display", family = "caption")
font_add_google("Patrick Hand", family = "year")
font_add_google("Stoke", family = "legend")


font_add('fa-reg', 'fonts/Font Awesome 6 Free-Regular-400.otf')
font_add('fa-brands', 'fonts/Font Awesome 6 Brands-Regular-400.otf')
font_add('fa-solid', 'fonts/Font Awesome 6 Free-Solid-900.otf')

showtext_auto()


plot <- data %>%
  ggplot() +
  geom_polygon(data = data, aes(fill = annual_average, x = long, y = lat, group = group) , size=0, alpha=0.9) +
  geom_text(data=centers, aes(x=x, y=y, label=id), color="white", size=3, alpha=0.6) +
  paletteer::scale_fill_paletteer_c("ggthemes::Orange",
                                    name = "Level of \nDrought") +
  ggthemes::theme_map() +
  facet_wrap(~ year, nrow = 6) +
  labs(title = "Drought Level in the United States",
       subtitle = "Climate change is becoming more apparent day by day. Accordingly, this map illustrates the level of drought in the US <span style = 'font-size:30pt;color:#E69F00;'>since 2000.</span> <br>
       The tone of the color for each hexbin is based on the Drought Severity and Coverage Index. While <span style = 'font-size:30pt;color:#E69F00;'>0</span> indicates none of the area is <br> abnormally dry or in drought, <span style = 'font-size:30pt;color:#E69F00;'>500</span> means that all of the area is in exceptional drought.",
       caption = "Source: National Integrated Drought Information System | Plot: <span style='font-family:fa-brands'>&#xf09b;</span> muhammetozkaraca <span style='font-family:fa-brands'>&#xf099;</span> muhammetozkrca | #TidyTuesday-Week 24") + 
  theme(
    plot.background = element_rect(fill = "white",colour = NA),
    panel.background = element_rect(fill = 'white', colour = 'white'),
    panel.border = element_blank(),
    strip.text.x = element_text(family = "year", size = 19),
    strip.background = element_rect(fill = 'white', colour = 'white'),
    plot.title = element_text(hjust = 0.5, family = "title", size = 41),
    plot.subtitle = element_markdown(hjust = 0.5, family = "subtitle", size = 29), # note that to color numbers differently, I used element_markdown not element_text function
    plot.caption = element_markdown(hjust = 0.5, family = "caption", size = 17),
    legend.position = c(0.75, 0.05),
    legend.direction = "horizontal",
    legend.key.width = unit(4.5, "line"),
    legend.key.height = unit(1, 'cm'), 
    legend.key.size = unit(1, 'cm'), 
    legend.title = element_text(family = "legend", size=14), 
    legend.text = element_text(size=10))

ggsave("hexbin-plot.png", height = 20, width = 25)





