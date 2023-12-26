library(tidyverse)
library(showtext)
library(showtextdb)
library(geofacet)
library(ggthemes)
library(ggtext)
library(stringr)

font_add_google("Domine", bold.wt = 700, family = "title")
font_add_google("Roboto Slab", family = "subtitle") 
font_add_google("Spectral", family = "caption")

font_add('fa-reg', 'fonts/Font Awesome 6 Free-Regular-400.otf')
font_add('fa-brands', 'fonts/Font Awesome 6 Brands-Regular-400.otf')
font_add('fa-solid', 'fonts/Font Awesome 6 Free-Solid-900.otf')
showtext_auto()

house <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-11-07/house.csv')

rep_votes_by_year <- house %>%
  filter(party == "REPUBLICAN") %>%
  group_by(state, year) %>%
  mutate(vote = sum(candidatevotes),
         total_vote = sum(totalvotes)) %>%
  select(state, party, vote) %>%
  ungroup() %>%
  distinct()

dem_votes_by_year <- house %>%
  filter(party == "DEMOCRAT") %>%
  group_by(state, year) %>%
  mutate(vote = sum(candidatevotes),
         total_vote = sum(totalvotes)) %>%
  select(state, party, vote) %>%
  ungroup() %>%
  distinct()


total_votes_by_states <- house %>%
  select(year, state, totalvotes) %>%
  distinct() %>%
  group_by(year, state) %>%
  mutate(total_votes = sum(totalvotes)) %>%
  select(year, state, total_votes) %>%
  distinct()


data <- rbind(rep_votes_by_year, dem_votes_by_year)
data_final <- total_votes_by_states %>%
  left_join(data, by = c("state", "year")) %>%
  mutate(percent = vote/total_votes,
         state = str_to_title(state),
         state = case_when(state == "District Of Columbia" ~ "District of<br>Columbia",
                           .default = as.character(state))) %>%
  filter(state != "Louisiana") 
  

grid <- geofacet::us_state_grid2
grid$name[grid$name == "District of Columbia"] <- "District of<br>Columbia"

plot <- ggplot(data_final,
       aes(x = year, y = percent, group = party, color = party)) +
  geom_line() +
  geom_point(data = data_final %>% filter(year == 2022), 
             aes(x = year, y = percent, group = party, color = party), size = 0.7) +
  geom_text(data = data_final %>% filter(year == 2022) %>% mutate(y_placement = percent, y_placement = ifelse(percent > 0.5, y_placement + 0.15, y_placement-0.15)), 
            aes(x = year, y = y_placement, label = round(percent, 2)), size = 10, nudge_x = -2,
            check_overlap = TRUE) +
  facet_geo(~state, grid = grid) +
  theme_fivethirtyeight() +
  labs(title = "US House Election Results",
       subtitle = "The plot demonstrates the percentage of votes <strong><span style='color: #0000FF;'>Democratic</span></strong> and <strong><span style='color: #FF0000;'>Republican</span></strong> candidates received in US House elections<br> between 1976 and 2022.",
       color = "Party",
       caption = "Source: MIT Election and Data Science Lab | Plot:<span style='font-family:fa-brands'>&#xf09b; </span>muhammetozkaraca <span style='font-family:fa-brands'>&#xf099;</span> @muhammetozkrca | #TidyTuesday-Week 45") +
  scale_color_manual(values = c("blue", "red"), 
                     labels = c("Democrat", "Republican"), 
                     breaks = c("DEMOCRAT", "REPUBLICAN")) +
  scale_x_continuous(limits = c(1976, 2022),
                     breaks = c(1976, 1988, 2000, 2012, 2022),
                     labels = c("1976", "1988", "2000", "2012", "2022")) +
  theme(plot.title = element_markdown(size = 120, family = "title"),
        plot.subtitle = element_markdown(size = 70, lineheight = 0.01, family = "subtitle"),
        plot.caption = element_markdown(linewidth = 0.02, hjust = 0.5, size = 45, family = "caption"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted"),
        axis.text.x = element_markdown(angle = 90, size = 35),
        axis.text.y = element_markdown(size = 35), 
        strip.text.x = element_markdown(size = 40, lineheight = 0.01, margin = margin(1,1,1,1, "pt")),
        legend.title = element_markdown(size = 65, face = "bold"),
        legend.text = element_markdown(size = 45),
        legend.direction = "vertical",
        legend.position = "none",
        legend.spacing.x = unit(0.01, "cm"),
        legend.spacing.y = unit(0.01, "cm"))



ggsave("/Users/muhammetozkaraca/Desktop/plot.png", dpi = 720,  height = 6, width = 8)















  