library(tidyverse)
library(countrycode)
library(circlize)
library(ggplot2)
library(cowplot)
library(grid)
library(ggplotify)
library(skimr)
library(patchwork)
library(showtext)
library(ggtext)
library(ggrepel)
library(glue)
library(fontawesome)

font_add_google("Martel", family = "title")
font_add_google("Libre Caslon Display", family = "subtitle")
font_add_google("Space Mono", family = "axis")
font_add_google("Spartan", family = "caption")

showtext_auto()

erasmus <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-08/erasmus.csv')

country_codes <- countrycode::codelist %>%
  select(iso2c, country_name = country.name.en)

erasmus %>%
  filter(sending_country_code != receiving_country_code) %>%
  mutate(total = sum(participants),
         country_number = length(unique(sending_country_code)),
         average_participant = total/country_number,
         average_age = mean(participant_age)) %>%
  select(average_participant, average_age) %>%
  distinct()

erasmus_tidied <- erasmus %>%
  filter(sending_country_code != receiving_country_code) %>%
  mutate(female = case_when(
    participant_gender == "Female" ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(female_total = female*participants) %>%
  select(-female) %>%
  group_by(sending_country_code) %>%
  mutate(average = mean(participant_age),
         total = sum(participants),
         female_prop = sum(female_total)/total) %>%
  left_join(country_codes, by = c("sending_country_code" = "iso2c")) %>%
  select(sending_country_code, country_name, average, total, female_prop) %>%
  mutate(country_name = case_when(
    sending_country_code == "UK" ~ "United Kingdom",
    sending_country_code == "EL" ~ "Greece",
    sending_country_code == "XK" ~ "Kosovo",
    TRUE ~ country_name)) %>%
  distinct()


plot <- ggplot(erasmus_tidied, aes(x = average, y = total, color = female_prop)) +
  geom_point() +
  geom_hline(yintercept = 318, alpha = 0.5, color = "grey") +
  geom_vline(xintercept = 24.3, alpha = 0.5, color = "grey") +
  geom_text_repel(label = erasmus_tidied$country_name, family = "mono", force = 2) +
  theme_bw() +
  labs(x = "Participant Age Average per Country",
       y = "Total Number of Participant per Country",
       title = "Erasmus Participation",
       subtitle = "Several countries have participated in the Erasmus program since its debut on 2014. This plot demonstrates the average number of participant age \n and the average number of participants from each country",
       color = "Female\nProportion",
       caption = "Muhammet Özkaraca") +
  background_grid(major = 'none', minor = "none") +
  scale_y_continuous(breaks = c(500,1000)) +
  theme(
    plot.title = element_text(hjust = 0.5, family = "title", size = 20),
    plot.subtitle = element_text(hjust = 0.5, family = "subtitle", size = 14),
    plot.caption = element_text(hjust = 0.8, family = "caption", size = 9),
    legend.position = c(0.9, 0.6),
    legend.title = element_text(family = "caption", hjust = 1, vjust = 0.7),
    legend.title.align = 0.5,
    axis.title.x = element_text(family = "axis"),
    axis.title.y = element_text(family = "axis"),
    panel.border = element_blank(),
    axis.ticks = element_blank())
  
ggsave("scatter-plot.png", height = 10, width = 14)
