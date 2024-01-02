library(tidyverse)
library(ggtext)
library(showtext)
library(showtextdb)
library(lubridate)
library(ggbrick)
library(waffle)
library(ggthemes)

font_add_google("Domine", bold.wt = 700, family = "title")
font_add_google("Roboto Slab", family = "subtitle") 
font_add_google("Caladea", family = "caption")

font_add('fa-reg', 'fonts/Font Awesome 6 Free-Regular-400.otf')
font_add('fa-brands', 'fonts/Font Awesome 6 Brands-Regular-400.otf')
font_add('fa-solid', 'fonts/Font Awesome 6 Free-Solid-900.otf')
showtext_auto()

drugs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-03-14/drugs.csv')

authorized_drugs_per_year <- drugs %>%
  filter(!is.na(marketing_authorisation_date)) %>%
  mutate(marketing_authorisation_year = year(marketing_authorisation_date)) %>%
  select(medicine_name, marketing_authorisation_year) %>%
  group_by(marketing_authorisation_year) %>%
  mutate(number_of_authorized_drugs = n()) %>%
  select(marketing_authorisation_year, number_of_authorized_drugs) %>%
  distinct() %>%
  filter(marketing_authorisation_year < 2023) %>%
  ggplot(aes(x = marketing_authorisation_year, y = number_of_authorized_drugs)) +
  geom_line() +
  labs(title = "1995-2022")

drug_area_freq <- drugs %>%
  mutate(therapeutic_area = gsub(";  ", ";", therapeutic_area)) %>%
  separate_rows(therapeutic_area, sep = ";") %>%
  count(therapeutic_area, name = "Frequency") %>%
  arrange(desc(Frequency)) %>%
  select(therapeutic_area, Frequency) %>%
  na.omit()
  

# Top 5 types are Diabetes Mellitus, Type 2; HIV Infections; Breast Neoplasms;
# Carcinoma, Non-Small-Cell Lung; Immunization    

drugs %>%
  filter(!is.na(marketing_authorisation_date)) %>%
  mutate(marketing_authorisation_year = year(marketing_authorisation_date),
         therapeutic_area = gsub(";  ", ";", therapeutic_area)) %>%
  separate_rows(therapeutic_area, sep = ";") %>%
  mutate(therapeutic_area = case_when(therapeutic_area == "Diabetes Mellitus, Type 2" ~ "Diabetes Mellitus, Type 2",
                                      therapeutic_area == "HIV Infections" ~ " HIV Infections",
                                      therapeutic_area == "Breast Neoplasms" ~ "Breast Neoplasms",
                                      therapeutic_area == "Carcinoma, Non-Small-Cell Lung" ~ "Carcinoma, Non-Small-Cell Lung",
                                      therapeutic_area == "Immunization" ~ "Immunization",
                                      .default = "Other")) %>%
  select(marketing_authorisation_year, therapeutic_area) %>%
  filter(therapeutic_area != "Other", marketing_authorisation_year > 2006) %>%
  group_by(marketing_authorisation_year) %>%
  count(therapeutic_area) %>%
  ggplot(aes(fill = therapeutic_area, values = n)) +
  geom_waffle(color = "white", size = .25, flip = TRUE, na.rm = TRUE) +
  facet_wrap(~marketing_authorisation_year, nrow = 4, strip.position = "top") +
  scale_x_discrete() +
  scale_y_continuous(labels = function(x) x * 10, # make this multiplyer the same as n_rows
                     expand = c(0,0)) +
  ggthemes::scale_fill_tableau(name=NULL) +
  coord_equal() +
  labs(title = "Drug Authorization in Europe",
       subtitle = "This plot demonstrates the annual distribution of top-5 drug areas per year between 2007-2022.",
       fill = "Gender",
       caption = "Source: European Medicines Agency | Plot:<span style='font-family:fa-brands'>&#xf09b; </span>muhammetozkaraca <span style='font-family:fa-brands'>&#xf099;</span> @muhammetozkrca | #TidyTuesday-Week 11") +
  theme_fivethirtyeight() +
  theme(plot.title = element_markdown(family = "title", size = 120),
        plot.subtitle = element_markdown(family = "subtitle", size = 90),
        plot.caption = element_markdown(hjust = 0.5, family = "caption", size = 60),
        strip.text = element_markdown(family = "caption", size = 80),
        legend.text = element_markdown(family = "caption", size = 45),
        panel.grid = element_blank(), 
        panel.grid.major.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank()) 

ggsave("/Users/muhammetozkaraca/Desktop/plot.png", dpi = 720)










