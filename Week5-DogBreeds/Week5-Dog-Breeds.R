library(tidyverse)
library(ggplot2)
library(showtext)
library(gt)
library(gtExtras)
library(ggtext)
library(tidytuesdayR)


breed_traits <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_traits.csv')
trait_description <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/trait_description.csv')
breed_rank_all <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_rank.csv')



rank <- breed_rank_all %>%
  select(Breed, `2020 Rank`, Image) %>%
  arrange(`2020 Rank`) %>%
  slice (1:5)

gt::gt(rank) %>%
  gt_theme_nytimes () %>%
  tab_header(title = html("<strong>2020 Top Breeds</strong>")) %>%
  gtExtras::gt_img_rows(columns = Image, height = 65)

breed_data <- left_join(breed_traits, breed_rank_all, by = "Breed")

# breed_data1 <- merge(breed_traits, breed_rank_all, by = "Breed")
# breed_data2 <- left_join(breed_traits, breed_rank_all, by = "Breed")
# breed_data3 <- inner_join(breed_traits, breed_rank_all, by = "Breed")
# breed_data4 <- full_join(breed_traits, breed_rank_all, by = "Breed")

breed_data <- right_join(breed_traits, breed_rank_all, by = "Breed")

breed_traits$Breed
breed_rank_all$Breed

all(breed_traits$Breed == breed_rank_all$Breed)


if (breed_traits$Breed == breed_rank_all$Breed)
{
  print("Column A and B are identical")
}

total <- merge(breed_traits, breed_rank_all, by = "Breed")





