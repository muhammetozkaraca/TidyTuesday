library(tidyverse)
library(waffle)
library(ggthemes)

haunted_places <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-10/haunted_places.csv')
haunted_places_top_5 <- haunted_places %>% 
  mutate(last_word = stringr::str_extract(location, "\\S+$"),
         last_word = str_to_title(last_word),
         last_word = case_when(last_word == "Rd." ~ "Road",
                               last_word == "Rd" ~ "Road",
                               last_word == "Streets" ~ "Street",
                               last_word == "Street)" ~ "Street", 
                               last_word == "St." ~ "Street", 
                               .default = as.character(last_word))) %>%
  group_by(last_word) %>%
  mutate(count = n(),
         percentage = count/10992,
         total_count = 10992) %>%
  select(last_word, count, total_count, percentage) %>%
  distinct() %>%
  arrange(desc(count))

haunted_places_top_5 <- haunted_places_top_5[1:5, ] 

ggplot(haunted_places_top_5, aes(fill = last_word, values = count)) +
  geom_waffle(color = "white", size = 0.01, na.rm = TRUE, nrows = 10) +
  facet_wrap(~last_word, nrow = 2) +
  theme_fivethirtyeight() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
        legend.position = "none") +
  coord_fixed(ratio = 1)
  

ggsave("/Users/muhammetozkaraca/Desktop/plot.png", dpi = 720,  height = 2, width = 3)

  



