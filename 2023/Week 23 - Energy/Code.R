library(tidyverse)
library(ggtext)
library(geofacet)
library(showtext)
library(showtextdb)


font_add_google("Amethysta", bold.wt = 700, family = "plottitle")
font_add_google("Roboto Slab", family = "title") 
font_add_google("Ubuntu", family = "caption")


font_add('fa-reg', 'fonts/Font Awesome 6 Free-Regular-400.otf')
font_add('fa-brands', 'fonts/Font Awesome 6 Brands-Regular-400.otf')
font_add('fa-solid', 'fonts/Font Awesome 6 Free-Solid-900.otf')

showtext_auto()


owid_energy <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-06/owid-energy.csv')

# renewables_share_energy -> Share of primary energy consumption that comes from renewables
# solar_cons_change_pct
europe_countries_grid2 <- europe_countries_grid2 %>%
  mutate(name = case_when(name == "UK" ~ "United Kingdom",
                          name == "Bosnia and Herzegovina" ~ "Bosnia",
                          name == "N. Macedonia" ~ "North Macedonia",
                          TRUE ~ name))


renewables_europe <- owid_energy %>% 
  mutate(country = case_when(country == "Bosnia and Herzegovina" ~ "Bosnia",
                             TRUE ~ country)) %>%
  filter(year >2000 & country %in% europe_countries_grid2$name & year < 2022) 

plot <- ggplot(renewables_europe, aes(x = year, y = 100)) + 
  geom_area(fill = "#99E9FF") +
  geom_area(aes(x = year, y = low_carbon_share_elec), fill = "#E6B63C") + 
  facet_geo(~ country, grid = "europe_countries_grid2") +
  theme_void() +
  labs(title = "Electricity Generation from Low-Carbon Sources (2001-2021)",
       subtitle = "<span style='color: #E6B63C;'>The shaded area</span> demonstrates the annual share of electricity generation from low carbon sources<br>(Renewables + Nuclear Energy)<br>",
       caption = "Source: Our World in Data | Plot: <span style='font-family:fa-brands'>&#xf09b;</span> muhammetozkaraca  <span style='font-family:fa-brands'>&#xf099;</span> muhammetozkrca | #TidyTuesday-Week 23") + 
  theme(plot.title = element_markdown(size = 110, hjust = 0.5, family = "plottitle", linewidth = 0.1),
        plot.subtitle = element_markdown(size = 80, hjust = 0.5, family = "title", lineheight = 0.2),
        plot.background = element_rect(fill = "#DDE6ED", color = NA),
        plot.caption = element_markdown(size = 55, hjust = 0.5, family = "caption", linewidth = 0.01),
        strip.text = element_markdown(size = 45, hjust = 0.5, family = "title"),
        panel.border = element_blank(),
        legend.title = element_markdown(size = 65, family = "caption", vjust = 0.75),
        legend.text = element_text(size = 40, family = "caption"),
        legend.spacing.x = unit(0.2, 'cm'),
        legend.spacing.y = unit(0.2, 'cm'),
        legend.direction = "horizontal",
        legend.position = c(0.2, 0.2))
  
ggsave("/Users/muhammetozkaraca/Desktop/low-carbon-sources.png", width = 8, height = 8, dpi = 720)

  
  
  