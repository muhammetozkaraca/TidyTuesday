library(tidyverse)
library(cowplot)
library(ggtext)
library(showtext)

font_add_google("Martel", family = "title")
font_add_google("Libre Caslon Display", family = "subtitle")
font_add_google("Space Mono", family = "axis")
font_add_google("Spartan", family = "caption")

showtext_auto()

tuesdata <- tidytuesdayR::tt_load('2022-03-22')
babynames <- tuesdata$babynames


babynames %>%
  group_by(year) %>%
  top_n(1, n) %>%
  ggplot(aes(x = year, y = prop)) +
  geom_step(size = 0.5, show.legend = FALSE, direction = "hv") +
  theme_bw() +
  labs(x = "Year", 
       y = "Proportion",
       title = "How Unique are Names in the United States?",
       subtitle = "This visualization illustrates the proportion of most given baby names in that year between 1880 and 2017",
       caption = "Source: {babynames} package | Plot: @muhammetozkrca | TidyTuesday-Week 12") +
  background_grid(major = 'none', minor = "none") +
  theme(
    plot.title = element_text(hjust = 0.5, family = "title", size = 20),
    plot.subtitle = element_markdown(hjust = 0.5, family = "subtitle", size = 14), # note that to color numbers differently, I used element_markdown not element_text function
    plot.caption = element_text(hjust = 0.5, family = "caption", size = 7),
    legend.position = c(0.9, 0.6),
    legend.justification = "center",
    legend.title = element_text(family = "caption", hjust = 1, vjust = 0.7),
    legend.title.align = 0.5,
    axis.title.x = element_text(family = "axis"),
    axis.title.y = element_text(family = "axis"),
    panel.border = element_blank(),
    axis.ticks = element_blank())


ggsave("geom_step-plot.png", height = 7, width = 11)
