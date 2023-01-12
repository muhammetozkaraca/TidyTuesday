library(tidyverse)
library(infer)



tuesdata <- tidytuesdayR::tt_load(2022, week = 6)
airmen <- tuesdata$airmen

airmen %>% count(pilot_type)

airmen <- airmen %>%
  mutate(pilot_type = str_replace_all(pilot_type, "Liason", "Liaison"))

airmen %>%
  ggplot(aes(pilot_type, fill = rank_at_graduation)) +
  geom_bar(stat = "count")


aircraft <- c("Single engine", "Twin engine")
ranks <- c("Flight Officer", "2nd Lt")

pilot_vs_rank <- 
  airmen %>%
  filter(pilot_type %in% aircraft, rank_at_graduation %in% ranks) %>%
  specify(pilot_type ~ rank_at_graduation, success = "Twin engine")

set.seed(234)

permuted <- 
  pilot_vs_rank %>%
  hypothesize(null = "independence") %>%
  generate (reps = 1000, type = "permute")



