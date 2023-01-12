library(tidyverse)
library(countrycode)
library(circlize)
library(ggplot2)
library(cowplot)
library(grid)
library(ggplotify)
library(patchwork)
library(showtext)
library(ggtext)
library(ggrepel)
library(paletteer)


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

paletteer_c(`"scico::bilbao"`, n=5)
paletteer_c(`"scico::davos"`, n=5)
paletteer_c(`"scico::acton"`, n=5)
paletteer_c(`"ggthemes::Classic Green-Blue"`, n=10)

plot <- ggplot(erasmus_tidied, aes(x = average, y = total, color = female_prop)) +
  geom_point() +
  geom_hline(yintercept = 318, alpha = 0.5, color = "grey") +
  geom_vline(xintercept = 23.3, alpha = 0.5, color = "grey") +
  geom_text_repel(label = erasmus_tidied$country_name, family = "mono", force = 2) +
  theme_bw() +
  scale_colour_paletteer_c("scico::batlow") +
  # scale_color_continuous(palette = "Pastel1") +
  # scale_colour_gradientn(colours = topo.colors(10)) +
  labs(x = "Participant Age Average per Country",
       y = "Total Number of Participant per Country",
       title = "Erasmus Participation",
       subtitle = "<span style='font-size:14pt'>Several countries have participated in the Erasmus program since its debut in 2014. According to the data between <br>
       2014 and 2020, while the participants' mean age is <span style='color:#0072B2;'>23.3</span>, the average number of participants from each country is 
       <span style='color:#D55E00;'>318</span>.
       </span>",
       color = "Female\nProportion",
       caption = "Data: Data.Europa | Plot: @muhammetozkrca | TidyTuesday-Week 10") +
  background_grid(major = 'none', minor = "none") +
  scale_y_continuous(breaks = c(250, 500, 750, 1000)) +
  scale_x_continuous(breaks = c(20, 22.5, 25, 27.5, 30)) +
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
  
ggsave("scatter-plot.png", height = 7, width = 11)
