library(tidyverse)
library(janitor)
library(plotly)
library(gghighlight)
library(ggtext)
library(showtext)


tuesdata <- tidytuesdayR::tt_load(2023, week = 7)
age_gaps <- tuesdata$age_gaps


font_add_google("Archivo", family = "title")
font_add_google("Nunito", family = "subtitle")
font_add_google("Martel", family = "axis")
font_add_google("Spartan", family = "caption")

font_add('fa-reg', 'fonts/Font Awesome 6 Free-Regular-400.otf')
font_add('fa-brands', 'fonts/Font Awesome 6 Brands-Regular-400.otf')
font_add('fa-solid', 'fonts/Font Awesome 6 Free-Solid-900.otf')

showtext_auto()

# Film Directors

age_gaps <- age_gaps %>%
  pivot_longer(actor_1_name:actor_2_age,
               names_prefix = "(actor|character)_", 
               names_to = c("actor", ".value"),
               names_sep = '_')

my_favorite_directors <- c("Christopher Nolan", "Martin Scorsese", "Steven Spielberg", "Woody Allen")


age_gaps %>%
  ggplot(aes(x = release_year, y = age_difference, color = director, alpha = 0.2)) +
  geom_point() +
  gghighlight(director %in% my_favorite_directors, use_direct_label = FALSE) +
  facet_wrap(~director) +
  scale_color_manual(values=c("#005f73", "#284b63", "#ca6702", "#9b2226")) +
  labs(title = "Age Difference between the Actors in My Favorite Directors' Movies",
       subtitle = "Data includes <span style='font-weight:bold; color:#ff9f1c;'>1155 Hollywood movies</span> that were released between <span style='color:#ff9f1c;'>1935</span> and <span style='color:#ff9f1c;'>2022</span>.",
       x = "Release Year",
       y = "Age Difference",
       caption = "Source: Hollywood Age Gap | Plot: <span style='font-family:fa-brands'>&#xf09b;</span> muhammetozkaraca <span style='font-family:fa-brands'>&#xf099;</span> muhammetozkrca | #TidyTuesday-Week 7") +
  theme(strip.background = element_blank(),
        strip.text = element_textbox(size = 50, color = "white", fill = "#5D729D", family = "caption", box.color = "#4A618C",
                                     halign = 0.5, linetype = 1, r = unit(5, "pt"), width = unit(1, "npc"),
                                     padding = margin(2, 0, 1, 0), margin = margin(3, 3, 3, 3)),
        legend.position = "none",
        axis.title.y = element_markdown(family = "axis", size = 55, linewidth = 0.2),
        axis.title.x = element_markdown(family = "axis", size = 55, linewidth = 0.2),
        axis.text.y = element_markdown(family = "axis", size = 35),
        axis.text.x = element_markdown(family = "axis", hjust = 0.43, size = 35),
        plot.title = element_markdown(family = "title", size = 95, hjust = 0.5, lineheight = 0.15, linewidth = 0.1),
        plot.subtitle = element_markdown(family = "subtitle", size = 75, hjust = 0.5, lineheight = 0.15, linewidth = 0.1),
        plot.caption = element_markdown(family = "title", size = 45, lineheight = 0.15, linewidth = 0.1, hjust = 0.5))
        

ggsave("/Users/muhammetozkaraca/Desktop/hollywood-movies.png", height = 6,  width = 7.5, dpi = 720)


age_gaps <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-14/age_gaps.csv') %>% 
  mutate(age_man = ifelse(character_1_gender == "man", actor_1_age, actor_2_age),
         age_woman = ifelse(character_1_gender == "woman", actor_1_age, actor_2_age)) %>%
  arrange(release_year)


age_gaps<- age_gaps %>%
  mutate(period = case_when(release_year > 1999 ~ 1,
                            release_year < 2000 ~ 0)) %>%
  mutate(period = as.factor(period))


ggplot(age_gaps, aes(x = age_man, y = age_woman, color = period, alpha = 0.5)) +
  geom_jitter() +
  scale_x_continuous(limits = c(min(age_gaps$age_man), max(age_gaps$age_man))) +
  scale_y_continuous(limits = c(min(age_gaps$age_woman), max(age_gaps$age_woman))) +
  geom_smooth(linetype = "dashed", color = "#023047", alpha = 0.7, se = FALSE) +
  scale_color_manual(values = c("#219ebc", "#fb8500"),
                     labels = c("Pre 2000s", "Post 2000s")) +
  guides(alpha = "none") + #to remove alpha legend from the visualization
  labs(title = "Male and Female Actors' Ages in Hollywood Movies",
       subtitle = "Data includes <span style='font-weight:bold; color:#ff9f1c;'>1155 Hollywood movies</span> that were released between <span style='color:#ff9f1c;'>1935</span> and <span style='color:#ff9f1c;'>2022</span>.",
       x = "Male Actor Age",
       y = "Female Actor Age",
       color = "Period", 
       caption = "Source: Hollywood Age Gap | Plot: <span style='font-family:fa-brands'>&#xf09b;</span> muhammetozkaraca <span style='font-family:fa-brands'>&#xf099;</span> muhammetozkrca | #TidyTuesday-Week 7") +
  theme_minimal() +
  theme(axis.title.y = element_markdown(family = "axis", size = 55, linewidth = 0.2),
        axis.title.x = element_markdown(family = "axis", size = 55, linewidth = 0.2),
        axis.text.y = element_markdown(family = "axis", size = 45),
        axis.text.x = element_markdown(family = "axis", hjust = 0.43, size = 45),
        plot.title = element_markdown(family = "title", size = 95, hjust = 0.5, lineheight = 0.15, linewidth = 0.1),
        plot.subtitle = element_markdown(family = "subtitle", size = 75, hjust = 0.5, lineheight = 0.15, linewidth = 0.1),
        plot.caption = element_markdown(family = "title", size = 45, lineheight = 0.15, linewidth = 0.1, hjust = 0.5),
        plot.background = element_rect(fill = "white"),
        # legend.position = c(0.05, 0.9), 
        legend.position = "top",
        legend.justification = c(0, 0),
        legend.spacing.x = unit(0.1, 'cm'), 
        legend.direction = "horizontal",
        legend.title = element_markdown(face="bold", family = "title", size = 55),
        legend.text = element_markdown(face="bold", family = "title", size = 45),
        legend.key = element_blank())

ggsave("/Users/muhammetozkaraca/Desktop/male-female-ages.png", height = 6,  width = 7.5, dpi = 720)
















