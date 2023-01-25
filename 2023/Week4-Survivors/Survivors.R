library(tidyverse)
library(ggtext)
library(showtext)
library(ggiraph)
library(relper)

tuesdata <- tidytuesdayR::tt_load(2023, week = 4)
survivalists <- tuesdata$survivalists

font_add_google('Merriweather', family = "title")
font_add_google('Averia Serif Libre', family = "subtitle")


font_add('fa-reg', 'fonts/Font Awesome 6 Free-Regular-400.otf')
font_add('fa-brands', 'fonts/Font Awesome 6 Brands-Regular-400.otf')
font_add('fa-solid', 'fonts/Font Awesome 6 Free-Solid-900.otf')

showtext_auto()

survivalists_season1 <- survivalists %>%
  filter(season == 1) %>%
  select(name, days_lasted, result) %>%
  mutate(active_days = map(days_lasted, ~(0:.))) %>%
  unnest(active_days) %>%
  mutate(name = str_replace(name, "Chris Weatherman", "C. Weatherman"),
         name = str_replace(name, "Wayne Russell", "W. Russell"))

ggplot(survivalists_season1, aes(x = reorder(name, desc(days_lasted)), y = active_days, fill = name)) +
  geom_tile(col = 'white', width = 0.3, height = 1) +
  expand_limits(x = 0, y = 0) +
  geom_hline(yintercept = c(0, 20, 40, 60), col = "grey80", alpha = 0.3, linewidth = 0.2) +
  geom_text(data = survivalists_season1 %>% group_by(name) %>% summarise(max = max(days_lasted)), 
            aes(x = name, y = max, label = max, family = "subtitle", fontface = "bold"), 
            color = "grey20", size = 25, vjust = -0.75) +
  theme_minimal() +
  scale_fill_manual(values=c("#31302D", "#655B4D", "#8C8274", "#B9A791", "#D2C5BE", "#91B3C7", 
                             "#7096AC", "#4E7890", "#3D5460", "lightgrey")) +
  labs(title = "How Many Days Survivors Lasted in Alone Season 1?",
       subtitle = "Alone is a TV survival series, where 10 survivalists are dropped in an extremenly remote area<br>and must fend for themselves. This plot demonstrates how many days particpants lasted<br>in Alone Season 1",
       x = "Participants",
       y = "Days Lasted",
       caption = "Source: ALONE by Daniel Oehm | Plot: <span style='font-family:fa-brands'>&#xf09b;</span> muhammetozkaraca <span style='font-family:fa-brands'>&#xf099;</span> muhammetozkrca | #TidyTuesday-Week 4 | Inspiration: @rappa753") +
  theme(plot.title = element_markdown(family = "title", size = 80, lineheight = 0.15, linewidth = 0.1, hjust = 0.5),
        plot.subtitle = element_markdown(family = "title", size = 45, lineheight = 0.15, linewidth = 0.1, hjust = 0.5),
        plot.caption = element_markdown(family = "title", size = 30, lineheight = 0.15, linewidth = 0.1, hjust = 0.5),
        axis.title.y = element_markdown(family = "subtitle", size = 55, linewidth = 0.2, angle = 90),
        axis.text.y = element_markdown(family = "subtitle", size = 50),
        axis.title.x = element_markdown(family = "subtitle", size = 45, linewidth = 0.2),
        axis.text.x = element_markdown(family = "subtitle", hjust = 0.43, size = 30),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        plot.background = element_rect(fill = "white", color = "white"),
        legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

ggsave("/Users/muhammetozkaraca/Desktop/survivors.png", height = 3,  width = 4.5, dpi = 720)


