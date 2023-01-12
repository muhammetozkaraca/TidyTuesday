library(tidyverse)
library(cowplot)
library(ggtext)
library(showtext)
library(streamgraph)
library(ggstream)
library(ggshadow)
library(shadowtext)
library(ggrepel)
library(monochromeR)
library(paletteer)

font_add_google("Martel", family = "title")
font_add_google("Libre Caslon Display", family = "subtitle")
font_add_google("Space Mono", family = "axis")
font_add_google("Spartan", family = "caption")


font_add('fa-reg', 'fonts/Font Awesome 6 Free-Regular-400.otf')
font_add('fa-brands', 'fonts/Font Awesome 6 Brands-Regular-400.otf')
font_add('fa-solid', 'fonts/Font Awesome 6 Free-Solid-900.otf')

showtext_auto()

tuesdata <- tidytuesdayR::tt_load('2022-03-22')
babynames <- tuesdata$babynames


babynames %>%
  group_by(year) %>%
  top_n(1, n) %>%
  ggplot(aes(x = year, y = prop)) +
  geom_step(size = 0.5, show.legend = FALSE, direction = "hv") +
  theme_bw() +
  labs(x = "Year", 
       y = "Proportion",
       title = "How Unique are Names in the United States?",
       subtitle = "This visualization illustrates the proportion of most given baby names in that year between 1880 and 2017",
       caption = "Source: {babynames} package | Plot: @muhammetozkrca | TidyTuesday-Week 12") +
  background_grid(major = 'none', minor = "none") +
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

ggsave("geom_step-plot.png", height = 7, width = 11)


paletteer_d(`"nord::frost"`)
paletteer_d(`"LaCroixColoR::Lime"`)
paletteer_d(`"dutchmasters::milkmaid"`)
paletteer_d(`"calecopal::sierra1"`)
paletteer_d(`"IslamicArt::samarqand"`)


labels <- babynames %>%
  filter(sex == "M") %>%
  group_by(name) %>%
  mutate(total = sum(n)) %>%
  distinct(name, total) %>%
  arrange(desc(total)) %>%
  head(5) 

labels_last <- babynames %>%
  filter(sex == "M", year == 2017, name %in% c("James", "John", "Robert", "Michael", "William"))

line_plot <- babynames %>%
  ggplot() +
  geom_line(data = babynames %>% filter(sex == "M"), 
            aes(x = year, y = prop, group = name), color = "#A59DA2FF", size = 0.1, alpha = 0.4) +
  geom_shadowline(data = babynames %>% filter(sex == "M", name %in% c("James", "John", "Robert", "Michael", "William")), 
                  aes(x = year, y = prop, group = name, color = name), size = 1, alpha = 1, shadowsize = 0.8) +
  # geom_point(data = labels_last, aes(x = year, y = prop), size = 1) +
  geom_text_repel(data = labels_last, aes(x = year, y = prop, label = name, color = name),
                  size = 5,
                  direction = "y",
                  xlim = c(2025, NA),
                  vjust = 0.5, 
                  segment.size = .7,
                  segment.alpha = .5,
                  segment.linetype = "dotted",
                  box.padding = .9,
                  segment.curvature = -0.1,
                  segment.ncp = 3,
                  segment.angle = 20
  ) +
  # geom_text_repel(), hjust = 0, vjust = 0.2, size = 3, family = "title") +
  # geom_shadowtext(data = labels_last, aes(x = year, y = prop, label = name, size = 11), check_overlap = TRUE, color = col1, bg.color = bg_col,
  #                hjust = 0, vjust = 0.2, nudge_x = 0.70, bg.r = 0.08) +
  scale_colour_manual(values=c(William = "#99985DFF", James = "#D9B196FF", Michael = "#1BB6AFFF",  
                               John = "#0076C0FF", Robert = "#172869FF")) +
  scale_y_continuous(expand = c(0,0),
                     breaks = c(seq(0.02, 0.08, by = 0.02))) +
  theme_bw() +
  labs(x = "Year", 
       y = "Proportion",
       title = "How Unique are Names in the United States?",
       subtitle = "This visualization illustrates the historical variation of the 5 most given male baby names' proportion between 1880 and 2017",
       caption = "Source: {babynames} package | Plot: <span style='font-family:fa-brands'>&#xf09b;</span> muhammetozkraca, <span style='font-family:fa-brands'>&#xf099;</span> muhammetozkrca | TidyTuesday-Week 12") +
  background_grid(major = 'none', minor = "none") +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, family = "title", size = 20),
    plot.subtitle = element_markdown(hjust = 0.5, family = "subtitle", size = 14), # note that to color numbers differently, I used element_markdown not element_text function
    plot.caption = element_markdown(hjust = 0.5, family = "caption", size = 7),
    axis.title.x = element_text(family = "axis"),
    axis.title.y = element_text(family = "axis"),
    panel.border = element_blank(),
    axis.ticks = element_blank())


ggsave("line_plot.png", height = 7, width = 12)


### Trying to reproduce the same plot with gghighlight


plot <- babynames %>% 
  ggplot() +
  geom_line(data = babynames %>% filter(sex == "M"), 
            aes(x = year, y = prop, group = name, color = name, label = name)) +
  scale_colour_manual(values=c(William = "#99985DFF", James = "#D9B196FF", Michael = "#1BB6AFFF",  
                               John = "#0076C0FF", Robert = "#172869FF")) +
  scale_y_continuous(expand = c(0,0),
                     breaks = c(seq(0.02, 0.08, by = 0.02))) +
  theme_bw() +
  labs(x = "Year", 
       y = "Proportion",
       title = "How Unique are Names in the United States?",
       subtitle = "This visualization illustrates the historical variation of the 5 most given male baby names' proportion between 1880 and 2017",
       caption = "Source: {babynames} package | Plot: <span style='font-family:fa-brands'>&#xf09b;</span> muhammetozkraca, <span style='font-family:fa-brands'>&#xf099;</span> muhammetozkrca | TidyTuesday-Week 12") +
  background_grid(major = 'none', minor = "none") +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, family = "title", size = 20),
    plot.subtitle = element_markdown(hjust = 0.5, family = "subtitle", size = 14), # note that to color numbers differently, I used element_markdown not element_text function
    plot.caption = element_markdown(hjust = 0.5, family = "caption", size = 7),
    axis.title.x = element_text(family = "axis"),
    axis.title.y = element_text(family = "axis"),
    panel.border = element_blank(),
    axis.ticks = element_blank())


plot + 
  geom_text_repel(data = labels_last, aes(x = year, y = prop, label = name, color = name),
                  size = 5,
                  direction = "y",
                  xlim = c(2025, NA),
                  vjust = 0.5, 
                  segment.size = .7,
                  segment.alpha = .5,
                  segment.linetype = "dotted",
                  box.padding = .9,
                  segment.curvature = -0.1,
                  segment.ncp = 3,
                  segment.angle = 20) +
  gghighlight(sex == "M", name %in% c("James", "John", "Robert", "Michael", "William"), use_direct_label = FALSE,
              unhighlighted_params = list(size = 0.1, colour = alpha("#A59DA2FF", 0.4))) 



ggsave("line_plot2.png", height = 7, width = 12)

### Streamgraph


top5_male <- babynames %>%
  filter (sex == "M") %>%
  group_by(name) %>%
  mutate(total = sum(n)) %>%
  select(name, total) %>%
  arrange(desc(total)) %>%
  distinct(name, total) %>%
  head(5)


babynames %>%
  filter(sex == "M", name %in% c("James", "John", "Robert", "Michael", "William")) %>%
  streamgraph("name", "n", "year") %>%
  sg_axis_x(10, "year", "%Y") %>%
  sg_legend(TRUE, label = "Name: ") %>%
  sg_fill_brewer("PuOr")


