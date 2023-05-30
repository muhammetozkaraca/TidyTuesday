library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(bertin)
library(usmap)
library(maps)
library(showtext)
library(showtextdb)
library(ggtext)
library(cowplot)
library(ggmagnify)

font_add_google("Cinzel", bold.wt = 700, family = "plottitle")
font_add_google("Bodoni Moda", family = "title")
font_add_google("Ubuntu", family = "caption")


font_add('fa-reg', 'fonts/Font Awesome 6 Free-Regular-400.otf')
font_add('fa-brands', 'fonts/Font Awesome 6 Brands-Regular-400.otf')
font_add('fa-solid', 'fonts/Font Awesome 6 Free-Solid-900.otf')

showtext_auto()

tuesdata <- tidytuesdayR::tt_load(2023, week = 17)
tornados <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-16/tornados.csv')

state_names_df <- tibble(state = state.name) %>%
  bind_cols(tibble(abb = state.abb)) %>% 
  bind_rows(tibble(state = "District Of Columbia", abb = "DC")) %>%
  as.data.frame()

maps_states_df <- st_as_sf(map("state", plot = FALSE, fill = TRUE)) %>%
  mutate(ID = str_to_title(ID)) 

us_geo_data <- maps_states_df %>%
  left_join(state_names_df, by = c("ID" = "state"))


tornados_summarised <- tornados %>%
  group_by(st) %>%
  summarise(count = n()) %>%
  left_join(us_geo_data, by = c("st" = "abb")) %>%
  na.omit(state) %>%
  filter(! st %in% c("AK", "HI")) %>%
  as.data.frame() %>%
  st_as_sf()

plot <- ggplot(data = us_geo_data) +
  geom_sf(aes(geometry = geom), linewidth = 0.1, show.legend = FALSE) +
  geom_density_2d_filled(data = tornados %>% filter(!st %in% c("VI", "DC", "AK", "PR", "HI")), 
                         aes(x = slon, y = slat), show.legend = FALSE, alpha = 0.6, bins = 10) +
  scale_fill_manual(values = c("#dedad2","#d7e1ee", "#cbd6e4", "#bfcbdb", "#b3bfd1", "#a4a2a8", "#df8879", "#c86558", "#b04238", "#991f17")) +
  ggthemes::theme_map() +
  labs(title = "Tornado Density in US",
       subtitle = "This map demonstrates the density of Tornados across the US states starting from 1950 to 2022.
       <br><span style='color: #991f17;'>Red areas</span> are the regions, where there are high frequency of tornados.",
       caption = "Data: NOAA | Plot: <span style='font-family:fa-brands'>&#xf09b;</span> muhammetozkaraca  <span style='font-family:fa-brands'>&#xf099;</span> muhammetozkrca | #TidyTuesday-Week 20") +
  theme(plot.title = element_markdown(size = 30, hjust = 0.5, family = "plottitle", linewidth = 0.1),
        plot.subtitle = element_markdown(size = 24, hjust = 0.5, family = "title", lineheight = 0.2),
        plot.background = element_rect(fill = "#dedad2", color = NA),
        plot.caption = element_markdown(hjust = 0.5, family = "caption", size = 16, linewidth = 0.01),
        panel.border = element_blank())

ggsave("/Users/muhammetozkaraca/Desktop/tornados-us1.png", width = 2, height = 2.5, dpi = 720)

font_add_google("Amethysta", bold.wt = 700, family = "plottitle2")
font_add_google("Roboto Slab", family = "title2")
font_add_google("Ubuntu", family = "caption")


font_add('fa-reg', 'fonts/Font Awesome 6 Free-Regular-400.otf')
font_add('fa-brands', 'fonts/Font Awesome 6 Brands-Regular-400.otf')
font_add('fa-solid', 'fonts/Font Awesome 6 Free-Solid-900.otf')

showtext_auto()


plot2 <- tornados_across_us + geom_magnify(aes(from = abb == "TX"),
                                  to = c(-79.5, -64.5, 24, 38), 
                                  shadow = TRUE, linewidth = 0.65, colour = "orange2",
                                  shape = "outline", #"ellipse" argument can also be used to make an ellipse shape inset plot. 
                                  aspect = "fixed", 
                                  expand = 0) +
  labs(title = "Geolocations of Tornados in the US",
       subtitle = "This map demonstrates the geolocations of Tornados occurred across the US states starting from 1950 to 2022.
       <br><span style='color: #991f17;'>Texas</span> is the primary state, exposed to more <span style='color: #991f17;'>9.000 tornados</span> between 1950 and 2022.",
       caption = "Data: NOAA | Plot: <span style='font-family:fa-brands'>&#xf09b;</span> muhammetozkaraca  <span style='font-family:fa-brands'>&#xf099;</span> muhammetozkrca | #TidyTuesday-Week 20") +
  theme(plot.title = element_markdown(size = 95, hjust = 0.5, family = "plottitle2", linewidth = 0.1),
        plot.subtitle = element_markdown(size = 70, hjust = 0.5, family = "title2", lineheight = 0.2),
        plot.background = element_rect(fill = "#dedad2", color = NA),
        plot.caption = element_markdown(size = 45, hjust = 0.5, family = "caption", linewidth = 0.01),
        panel.border = element_blank())


ggsave("/Users/muhammetozkaraca/Desktop/tornados-us2.png", dpi = 720)






