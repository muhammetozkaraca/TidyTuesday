library(tidytuesdayR)
library(tidyverse)
library(ggthemes)
library(patchwork)
library(showtext)
library(showtextdb)
library(ggtext)

font_add_google("Lora", bold.wt = 700, family = "plottitle")
font_add_google("Roboto Slab", family = "title") 
font_add_google("Ubuntu", family = "caption")

font_add('fa-reg', 'fonts/Font Awesome 6 Free-Regular-400.otf')
font_add('fa-brands', 'fonts/Font Awesome 6 Brands-Regular-400.otf')
font_add('fa-solid', 'fonts/Font Awesome 6 Free-Solid-900.otf')
showtext_auto()

holiday_movies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-12-12/holiday_movies.csv')
holiday_movie_genres <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-12-12/holiday_movie_genres.csv')

comedy_movies <- holiday_movies %>%
  filter(genres %in% c("Comedy")) %>%
  arrange(year) %>%
  drop_na() %>%
  ggplot(aes(x = year, y = runtime_minutes)) +
  geom_area(fill = "lightblue", alpha = 0.5) + # Fills the area under the line
  geom_line(color = "blue") +
  labs(title = "Comedy") +
  scale_y_continuous(limits = c(0, 250)) +
  scale_x_continuous(limits = c(1929, 2023)) +
  theme_fivethirtyeight() 

animation_movies <- holiday_movies %>%
  filter(genres %in% c("Animation")) %>%
  arrange(year) %>%
  drop_na() %>%
  ggplot(aes(x = year, y = runtime_minutes)) +
  geom_area(fill = "lightblue", alpha = 0.5) + # Fills the area under the line
  geom_line(color = "blue") +
  labs(title = "Animation") +
  scale_y_continuous(limits = c(0, 250)) +
  scale_x_continuous(limits = c(1929, 2023)) +
  theme_fivethirtyeight() 

drama_movies <- holiday_movies %>%
  filter(genres %in% c("Drama")) %>%
  arrange(year) %>%
  drop_na() %>%
  ggplot(aes(x = year, y = runtime_minutes)) +
  geom_area(fill = "lightblue", alpha = 0.5) + # Fills the area under the line
  geom_line(color = "blue") +
  labs(title = "Drama") +
  scale_y_continuous(limits = c(0, 250)) +
  scale_x_continuous(limits = c(1929, 2023)) +
  theme_fivethirtyeight() 

horror_movies <- holiday_movies %>%
  filter(genres %in% c("Horror")) %>%
  arrange(year) %>%
  drop_na() %>%
  ggplot(aes(x = year, y = runtime_minutes)) +
  geom_area(fill = "lightblue", alpha = 0.5) + # Fills the area under the line
  geom_line(color = "blue") +
  labs(title = "Horror") +
  scale_y_continuous(limits = c(0, 250)) +
  scale_x_continuous(limits = c(1929, 2023)) +
  theme_fivethirtyeight() 

romance_movies <- holiday_movies %>%
  filter(genres %in% c("Romance")) %>%
  arrange(year) %>%
  drop_na() %>%
  ggplot(aes(x = year, y = runtime_minutes)) +
  geom_area(fill = "lightblue", alpha = 0.5) + # Fills the area under the line
  geom_line(color = "blue") +
  labs(title = "Romance") +
  scale_y_continuous(limits = c(0, 250)) +
  scale_x_continuous(limits = c(1929, 2023)) +
  theme_fivethirtyeight() 


main_plot <- comedy_movies + animation_movies + drama_movies + horror_movies + romance_movies +
  plot_annotation(title = "Holiday Movies",
                  subtitle = "This plot demonstrates holiday movies' release years and running times (in minutes) based on their genres.",
                  caption = "Source: Internet Movie Database | Plot:<span style='font-family:fa-brands'>&#xf09b; </span>muhammetozkaraca <span style='font-family:fa-brands'>&#xf099;</span> @muhammetozkrca | #TidyTuesday-Week 50") &
  theme_fivethirtyeight() +
  theme(panel.grid.major.x = element_blank(),
        plot.title = element_markdown(size = 120),
        plot.subtitle = element_markdown(size = 70),
        axis.text.x = element_markdown(size = 45),
        axis.text.y = element_markdown(size = 45),
        plot.caption = element_markdown(linewidth = 0.02, hjust = 0, size = 45))

  
  
ggsave("/Users/muhammetozkaraca/Desktop/plot.png", dpi = 720,  height = 6, width = 8)

    
    




