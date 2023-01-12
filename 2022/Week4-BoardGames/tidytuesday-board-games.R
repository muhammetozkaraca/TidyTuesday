library(tidyverse)
library(ggplot2)
library(gt)
library(showtext) # to add special fonts from google
library(ggtext)
library(ggstream)
library(streamgraph)
library(gt)
library(gtExtras)


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

### Categories Plot ###


board_games$boardgamecategory <- substring(board_games$boardgamecategory,3,nchar(board_games$boardgamecategory)-2)
board_games$boardgamecategory <- str_replace_all(board_games$boardgamecategory, c("'" = ""))


splitted_data <-separate(board_games, col = boardgamecategory, 
                          into = c("categories1","categories2","categories3",
                                   "categories4","categories5","categories6",
                                   "categories7","categories8","categories9",
                                   "categories10","categories11","categories12",
                                   "categories13","categories14"), sep=",") 


# sum(!is.na(splitted_data$categories15)) checking whether all values are na or not



top_categories <- splitted_data %>%  
  pivot_longer(cols = categories1:categories14, names_to = "topcategories", values_to = "categoriestype", values_drop_na = TRUE) %>%
  select(-c(topcategories)) %>%
  group_by(categoriestype) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Top 5 categories are: Card Game, War Game, Fantasy, Party Game, Abstract Strategy

top_categories_data <- splitted_data %>%
  pivot_longer(cols = categories1:categories14, names_to = "topcategories", values_to = "categoriestype", values_drop_na = TRUE) %>%
  select(-c(topcategories)) %>%
  filter(categoriestype %in% c("Card Game", " Wargame", " Fantasy", " Party Game", "Abstract Strategy")) %>%
  group_by(yearpublished, categoriestype) %>%
  mutate(count = n()) %>%
  select(categoriestype, yearpublished, count) %>%
  distinct(categoriestype, .keep_all = TRUE) %>%
  as.data.frame() %>%
  filter(yearpublished > 1949) %>%
  arrange(desc(yearpublished), categoriestype)


top_categories_data$categoriestype <- trimws(top_categories_data$categoriestype)
top_categories_data$categoriestype <- as.factor(top_categories_data$categoriestype)


# pp <- streamgraph(top_categories_data, key="categoriestype", value="count", date="yearpublished", 
#                  height="300px", width="1000px")


# top_categories_data %>%
#  streamgraph(key="categoriestype", value="count", date="yearpublished", offset="zero") %>%
#  sg_fill_brewer("BuPu")

  

ggplot(top_categories_data, aes(x = yearpublished, y = count, 
                                fill = categoriestype)) + 
  geom_stream(type = "ridge", alpha = 0.8) +
  scale_fill_brewer("BuPu") +
  guides(fill=guide_legend(title="")) +
  theme_minimal() +
  scale_x_continuous(
    breaks = c(1950, 1960, 1970, 1980, 1990, 2000, 2010, 2020)
  ) +
  labs(title = "Distribution of New Board Games' Categories", 
       subtitle = "<span style = 'font-size:12pt'>Board games are an important part of daily life. Thanks to them, not only
    children but also adults can spend time having fun. This graph is prepared to visualize the top 5 categories of new board games published
    <span style='color:#B22222'>since 1950</span>.",
       # subtitle = "Evolution of the Games",
       caption = "#TidyTuesday Challenge | Muhammet Ozkaraca",
       x = "Year - Published",
       y = "Number of Board Games") +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(family = "title",
                              size = 14),
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
    axis.title.y = element_text(family = "title1"),
    legend.position = "top"
  ) 
  













