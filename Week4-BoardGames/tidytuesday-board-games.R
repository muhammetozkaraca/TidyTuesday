library(tidyverse)
library(ggplot2)
library(gt)
library(showtext) # to add special fonts from google
library(ggtext)

ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-25/ratings.csv')
details <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-25/details.csv')

board_games <-
  ratings %>%
  left_join(details, by = "id")


font_add_google("IBM Plex Mono", family = "title") #find the name of font from google, call it and name it in family argument to use locally
font_add_google("Pacifico", family = "title1") 
font_add_google("IBM Plex Serif", family = "axis-labels")
font_add_google("Monofett", family = "axis")
font_add_google("Syne Mono", family = "subtitle")
font_add_google("Abel", family = "subtitle1")

showtext_auto() # to invoke text to be used on the graph

a <- board_games %>%
  group_by(yearpublished) %>%
  mutate(count = n()) %>%
  arrange(yearpublished) %>%
  select(yearpublished, count) %>%
  distinct() %>%
  filter(yearpublished > 1899) 

sum(a$count)

### Density Plot ###

board_games %>%
  group_by(yearpublished) %>%
  mutate(count = n()) %>%
  arrange(yearpublished) %>%
  select(yearpublished, count) %>%
  distinct() %>%
  filter(yearpublished > 1899) %>%
  ggplot(aes(yearpublished, count)) + geom_area(fill = "aquamarine3", show.legend = FALSE, alpha = 0.8) +
  labs(title = "Board Games since 1900", 
  subtitle = "<span style = 'font-size:12pt'>Board games are an important part of daily life. Thanks to them, not only
    children but also adults can spend time having fun. This graph is prepared to visualize new board games published
    <span style='color:#B22222'>since 1930</span>. In total, there are <span style='color:#B22222'>**21331**</span> new board games depicted in this graph based on their published year.",
       # subtitle = "Evolution of the Games",
       caption = "#TidyTuesday Challenge | Muhammet Ozkaraca",
       x = "Year - Published",
       y = "Count") +
  theme_minimal() +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(family = "title",
                              size = 16),
    plot.subtitle = element_textbox_simple(
      family = "subtitle1",
      size = 12,
      lineheight = 1,
      padding = margin(5.5, 5.5, 5.5, 5.5),
      margin = margin(-6, 0, 5.5, -4),
      fill = "white"
    ),
    # plot.subtitle = element_markdown(valign = TRUE, hjust = 0, halign = TRUE, linewidth = 1),
    # plot.subtitle = element_textbox_simple(size = 12, lineheight = 1, padding = margin(0, 0, 5, 0),
    #                                       family = "subtitle"),
    plot.caption = element_text(family = "subtitle"),
    axis.text.x = element_text(family = "title"),
    axis.text.y = element_text(family = "title"),
    axis.title.x = element_text(family = "title1"),
    axis.title.y = element_text(family = "title1")
  ) +
  geom_curve(aes(x = 1996, y = 0, xend = 1996, yend = 950),
             curvature = 0.3,
             color = "darkgoldenrod1", size = 1,
             arrow = arrow(length = unit(0.03, "npc"),
                           type = "closed")) +
  annotate(geom = "text", x = 1988, y = 1070, hjust = 0.2, size = 4, color = "firebrick1", label = "1996 is the year \n I was born", family = "subtitle", fontface = "bold") 



# What I could not succeed?
 
## Find a way to make '1930' bold. I tried ** but did not work.


















































