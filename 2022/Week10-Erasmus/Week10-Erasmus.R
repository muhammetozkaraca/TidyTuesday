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

erasmus <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-08/erasmus.csv')

country_codes <- countrycode::codelist %>%
  select(iso2c, country_name = country.name.en)


font_add_google("Noto Serif Display", family = "title")
font_add_google("Libre Caslon Display", family = "subtitle")
font_add_google("Viaoda Libre", family = "caption")
showtext_auto()

top_incoming <- erasmus %>%
  filter (receiving_country_code == "TR") %>%
  filter (sending_country_code != receiving_country_code) %>%
  group_by(sending_country_code) %>%
  summarize(total = sum(participants)) %>%
  arrange(desc(total)) %>%
  filter(total>9) %>%
  left_join(country_codes, by = c("sending_country_code" = "iso2c")) %>%
  mutate(country_name = case_when(
    sending_country_code == "UK" ~ "United Kingdom",
    sending_country_code == "EL" ~ "Greece",
    TRUE ~ country_name
  )) %>%
  mutate(country_name = factor(country_name, levels = rev(country_name)))

top_going <- erasmus %>%
  filter (sending_country_code == "TR") %>%
  filter (sending_country_code != receiving_country_code) %>%
  group_by(receiving_country_code) %>%
  summarize(total = sum(participants)) %>%
  arrange(desc(total)) %>%
  left_join(country_codes, by = c("receiving_country_code" = "iso2c")) %>%
  filter(total>9) %>%
  mutate(country_name = case_when(
    receiving_country_code == "EL" ~ "Greece",
    receiving_country_code == "UK" ~ "United Kingdom",
    TRUE ~ country_name
  )) %>%
  mutate(country_name = factor(country_name, levels = rev(country_name)))


p1 <- ggplot(data = top_incoming,
            aes(x = country_name, y = total, fill = country_name)) +
  geom_bar(width = 0.3, stat = "identity", show.legend = FALSE) +
  geom_text(aes(x = country_name, y = 0, label = paste0(country_name, " - ", total)),
            hjust = 1.05, size = 2, colour = rep(c('#FA8334', '#42047E'), 10)) +
  coord_polar(theta = "y", start = 0) +
  ylim(c(0, 100)) +
  scale_fill_manual(values = rep(c('#FA8334', '#42047E'), 10)) +
  labs(title = "Incoming Students",
       subtitle = "(2015-2020)") +
  theme_void() +
  theme (plot.title = element_text(hjust = 0.5, family = "subtitle", size = 18),
         plot.subtitle = element_text(hjust = 0.5, family = "subtitle", size = 12))


p2 <- ggplot(data = top_going,
       aes(x = country_name, y = total, fill = country_name)) +
  geom_bar(width = 0.3, stat = "identity", show.legend = FALSE) +
  scale_fill_manual(values = rep(c('#FA8334', '#42047E'), 10)) +
  geom_text(aes(x = country_name, y = 0, label = paste0(country_name, " - ", total)),
            hjust = 1.05, size = 2, colour = rep(c('#FA8334', '#42047E'), 9)) +
  coord_polar(theta = "y", start = 0) +
  ylim(c(0, 100)) +
  labs(title = "Outgoing Students",
       subtitle = "(2014-2020)") +
  theme_void() +
  theme (plot.title = element_text(hjust = 0.5, family = "subtitle", size = 18),
         plot.subtitle = element_text(hjust = 0.5, family = "subtitle", size = 12))



circular_bar <- p1 + p2 +
  plot_annotation(
    title = "Incoming and Outgoing Students in  Turkey as part of Erasmus Program",
    subtitle = "Turkey is one of the participating countries in the Erasmus Program. This visualisation demonstrates \n the total number of incoming and outgoing students in participating Turkish institutions",
    caption = "Data: Data.Europa | Plot: @muhammetozkrca | TidyTuesday-Week 10",
    theme = theme(plot.title = element_text(hjust = 0.5, family = "title", size = 20),
                  plot.subtitle = element_text(hjust = 0.5, family = "subtitle", size = 14),
                  plot.caption = element_text(hjust = 0.5, family = "caption", size = 9)))

ggsave("circular_bar.jpg", height=9, width=11)


