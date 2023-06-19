library(tidyverse)
library(ggbeeswarm)
library(ggtext)
library(showtext)
library(showtextdb)
library(glue)


font_add_google("Lora", bold.wt = 700, family = "plottitle")
font_add_google("Roboto Slab", family = "title") 
font_add_google("Ubuntu", family = "caption")


font_add('fa-reg', 'fonts/Font Awesome 6 Free-Regular-400.otf')
font_add('fa-brands', 'fonts/Font Awesome 6 Brands-Regular-400.otf')
font_add('fa-solid', 'fonts/Font Awesome 6 Free-Solid-900.otf')
showtext_auto()

safi_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-13/safi_data.csv')


# label1 <- "<span style='color: #336B87; font-weight:bold;'>SAFI (Studying African Farmer-Led Irrigation)</span> is a project that is currently underway and<br>focuses on <span style='color: #336B87;'>farming and irrigation methods</span>.The data presented here is from a survey<br>conducted in Tanzania and Mozambique. The survey was conducted between<br>November 2016 and June 2017 using forms downloaded to Android Smartphones."


summary_data <- safi_data %>%
  group_by(village) %>%
  mutate(mean = mean(years_liv)) %>%
  select(village, mean) %>%
  distinct()

plot <- ggplot(safi_data) +
  # geom_beeswarm(cex = 3, color = "#763626") +
  geom_point(mapping = aes(y = years_liv, x = village, fill = respondent_wall_type), 
             color="white", shape=21,
             position = position_beeswarm(cex = 4.5),
             size = 6) +
  geom_point(data = summary_data, mapping = aes(x = village, y = mean), size=4.5, shape=23, fill = "#F79327", color="white") +
  # geom_quasirandom(color = "#763626", method = "pseudorandom") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Which Village's Interviewees Stayed Longer?",
       subtitle = "The plot demonstrates the number of years the SAFI project interviewees lived in the three villages of Ruaca, God and Chirodzo.",
       y = "Years Lived In",
       x = "",
       caption = "Source: SAFI Project | Inspiration: <span style='font-family:fa-brands'>&#xf099;</span> @tanya_shapiro | Plot: <span style='font-family:fa-brands'>&#xf09b;</span> muhammetozkaraca <span style='font-family:fa-brands'>&#xf099;</span> @muhammetozkrca | #TidyTuesday-Week 24") +
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100),
                     limits = c(0, 100),
                     labels = c("0", "25", "50", "75", "100")) +
  scale_x_discrete(labels=c("**Ruaca**<br>(n=49)", "**God**<br>(n=43)", "**Chirodzo**<br>(n=39)")) +
  scale_fill_manual(values = c("#884A39", "#EA906C", "#B31312", "#2B2A4C"),
                    labels = c("Burntbricks", "Cement", "Muddaub", "Sunbricks"),
                    name = "Respondent Wall Type") +
  geom_hline(yintercept = c(0, 25, 50, 75, 100), color = "grey", size = 0.5, alpha = 0.5) +
  # geomtextpath::geom_texthline(yintercept = mean(safi_data$years_liv), color="grey20", linetype="dashed",
  #                             label = glue("Avg: {round(mean(safi_data$years_liv),0)}\nYears"), 
  #                             hjust = 0.75, vjust = -0.5, lineheight = 0.28,
  #                             size = 10) +
  geom_segment(mapping=aes(y = 26, yend = 31.2, x = 2.95, xend = 2.35), linewidth=0.15) +
  geom_segment(mapping=aes(y = 21.8, yend = 31.2, x = 2, xend = 2.25), linewidth=0.15) +
  geom_segment(mapping=aes(y = 25.3, yend = 33, x = 1, xend = 2.2), linewidth=0.15) +
  geom_text(label = "Avg. Years\nLived In", x = 2.3, y = 36, family = "title", size = 20, lineheight = 0.15) +
  # geom_richtext(aes(x = 1.7, y = 55, label = label1), size = 17, family = "caption", hjust = 0, fill = NA, label.color = NA, lineheight = 0.2) +
  theme(panel.grid = element_blank(), 
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        plot.title = element_markdown(size = 100, hjust = 0, family = "title", face = "bold", linewidth = 0.3),
        plot.subtitle = element_markdown(size = 65, hjust = 0, family = "title", lineheight = 0.2),
        plot.caption = element_markdown(size = 45, hjust = 0.5, family = "caption", linewidth = 0.01),
        axis.text.y = element_markdown(size = 50, family = "caption", color = "grey60", face = "bold", lineheight = 0.1, hjust = 0),
        axis.title.x =  element_markdown(size = 50, family = "title", face = "bold"),
        axis.text.x = element_markdown(size = 50, family = "caption", face = "bold", lineheight = 0.1),
        axis.line.x = element_line(linewidth = 0.5, color="black"),
        panel.border = element_blank(),
        legend.position="top",
        legend.spacing.x = unit(0.2, 'cm'),
        legend.justification = c(0, 0),
        legend.title = element_markdown(face="bold", family = "title", size = 45),
        legend.text = element_markdown(size = 40, hjust = 0, family = "title"),
        legend.key = element_blank())
  

ggsave("/Users/muhammetozkaraca/Desktop/plot.png", dpi = 720,  height = 6, width = 8)







