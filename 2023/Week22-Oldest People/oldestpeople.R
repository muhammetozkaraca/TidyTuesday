library(tidyverse)
library(ggplot2)
library(tidytuesdayR)
library(highcharter)
library(maps)
library(jsonlite) # Load the 'jsonlite' package
library(showtext)
library(showtextdb)
library(ggtext)
library(paletteer)


font_add_google("Roboto", bold.wt = 700, family = "plottitle")
font_add_google("Bodoni Moda", family = "title")
font_add_google("Ubuntu", family = "caption")


font_add('fa-reg', 'fonts/Font Awesome 6 Free-Regular-400.otf')
font_add('fa-brands', 'fonts/Font Awesome 6 Brands-Regular-400.otf')
font_add('fa-solid', 'fonts/Font Awesome 6 Free-Solid-900.otf')

showtext_auto()

world_grid <- read_csv("worldtilegrid.txt") #file is available to download at https://www.maartenlambrechts.com/2017/10/22/tutorial-a-worldtilegrid-with-ggplot2.html
tt <- tidytuesdayR::tt_load('2023-05-30')
tt <- tt$centenarians 
countries_count <- tt %>%
  group_by(place_of_death_or_residence) %>%
  mutate(place_of_death_or_residence = case_when(place_of_death_or_residence == "France (French Guiana)" ~ "France",
                                                 place_of_death_or_residence == "France (Martinique)" ~ "France",
                                                 place_of_death_or_residence == "France (Saint Barthélemy)" ~ "France",
                                                 place_of_death_or_residence == "United States" ~ "United States of America",
                                                 place_of_death_or_residence == "United Kingdom" ~ "Great Britain and Northern Ireland",
                                                 place_of_death_or_residence == "Puerto Rico" ~ "United States of America",
                                                 TRUE ~ place_of_death_or_residence)) %>%
  summarise(count = n()) %>%
  as.data.frame()
  
data_to_plot <- world_grid %>%
  left_join(countries_count, by = c("name" = "place_of_death_or_residence")) %>%
  mutate(category = case_when(count > 0 & count <= 5 ~ "1-5", 
                              count > 5 & count <= 10 ~ "6-10", 
                              count > 10 & count <=15 ~ "11-15", 
                              count > 15 ~ "15+", 
                              TRUE ~ "0"),
         category = factor(category, levels = c("0", "1-5", "6-10", "11-15", "15+")))
  
a <- paletteer::palettes_d_names %>%
  filter(type == "sequential")

plot <- ggplot() +
  geom_tile(data = data_to_plot, aes(x = x, y = y, fill = category), color = "#ffffff") +
  geom_text(data = data_to_plot, aes(x = x, y = y, label = alpha.2), color = "#ffffff", alpha = 0.5, family = "caption", size = 25) +
  scale_y_reverse() + 
  theme_void() +
  scale_fill_manual(values = c("grey", "#C5CAE9", "#9FA8DA",  "#8C9EFF", "#3D5AFE")) +
  labs(fill = "Count",
       title = "Verified Oldest People Around the World",
       subtitle = "This map demonstrates the distribution of the place of birth or residence<br>of the verified 200 oldest people across countries.",
       caption = "Data: @frankiethull | Inspiration: Maarten Lambrechts | Plot: <span style='font-family:fa-brands'>&#xf09b;</span> muhammetozkaraca  <span style='font-family:fa-brands'>&#xf099;</span> muhammetozkrca | #TidyTuesday-Week 22") +
  theme(plot.title = element_markdown(size = 110, hjust = 0.5, family = "plottitle", linewidth = 0.1),
        plot.subtitle = element_markdown(size = 80, hjust = 0.5, family = "title", lineheight = 0.2),
        plot.background = element_rect(fill = "#dedad2", color = NA),
        plot.caption = element_markdown(size = 65, hjust = 0.5, family = "caption", linewidth = 0.01),
        panel.border = element_blank(),
        legend.title = element_markdown(size = 65, family = "caption", vjust = 0.75),
        legend.text = element_text(size = 40, family = "caption"),
        legend.spacing.x = unit(0.2, 'cm'),
        legend.spacing.y = unit(0.2, 'cm'),
        legend.direction = "horizontal",
        legend.position = c(0.2, 0.2))

ggsave("/Users/muhammetozkaraca/Desktop/oldest-people.png", width = 8, height = 8, dpi = 720)

