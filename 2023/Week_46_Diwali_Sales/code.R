library(tidyverse)
library(ggbeeswarm)
library(ggthemes)
library(ggtext)
library(showtext)
library(showtextdb)
library(scales)

font_add_google("Domine", bold.wt = 700, family = "title")
font_add_google("Roboto Slab", family = "subtitle") 
font_add_google("Caladea", family = "caption")

font_add('fa-reg', 'fonts/Font Awesome 6 Free-Regular-400.otf')
font_add('fa-brands', 'fonts/Font Awesome 6 Brands-Regular-400.otf')
font_add('fa-solid', 'fonts/Font Awesome 6 Free-Solid-900.otf')
showtext_auto()



house <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-11-14/diwali_sales_data.csv')
house$`Age Group` <- factor(house$`Age Group`, levels = c("0-17", "18-25", "26-35", "36-45", "46-50", "51-55", "55+"))
label_formatter <- function(x) {
  paste0(x / 1000, " K")
}

house %>%
  group_by(Gender, `Age Group`) %>%
  mutate(n = n()) %>%
  select(Gender, `Age Group`, n) %>%
  distinct()


plot <- ggplot(house) +
  geom_jitter(aes(x = `Age Group`, y = Amount, color = Gender), 
              position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.5), shape = 18, size = 2.3, alpha = 0.7) +
  coord_flip() +
  theme_fivethirtyeight() +
  labs(title = "Diwali Sales Data",
       subtitle = "<strong><span style='color: #FF9800;'>Diwali</span></strong>, a major Hindu festival, is commonly referred to as the <strong><span style='color: #FF9800;'>Festival of Lights</span></strong> . This plot illustrates the sales data from<br>a retail store during the festival period. Each point represents a unique customer, characterized by their <strong><span style='color: #143642;'>Age Group</span></strong>,<br><strong><span style='color: #750E21;'>the Amount of Sale</span></strong>, and <strong><span style='color: #0F8B8D;'>Gender</span></strong>. Note that <strong><span style='color: #750E21;'>the Amount of Sale</span></strong> variable is experessed in Indian Rupees.",
       fill = "Gender",
       caption = "Source: Saad Haroon | Plot:<span style='font-family:fa-brands'>&#xf09b; </span>muhammetozkaraca <span style='font-family:fa-brands'>&#xf099;</span> @muhammetozkrca | #TidyTuesday-Week 46") +
  scale_color_manual(values = c("#d90429", "#2b2d42"), 
                     labels = c("Female", "Male"), 
                     breaks = c("F", "M")) +
  scale_y_continuous(breaks = c(0, 5000, 10000, 15000, 20000, 25000), 
                     labels = label_formatter) +
  scale_x_discrete(labels=c("**0-17**<br>(F=167, M=134)", "**18-25**<br>(F=1305, M=574)", "**26-35**<br>(F=3271, M=1272)", "**36-45**<br>(F=1581, M=705)", "**46-50**<br>(F=696, M=291)", "**51-55**<br>(F=554, M=278)", "**55+**<br>(F=273, M=155)")) +
  theme(plot.title = element_markdown(size = 120, family = "title"),
        plot.subtitle = element_markdown(size = 70, lineheight = 0.1, family = "subtitle"),
        plot.caption = element_markdown(linewidth = 0.01, hjust = 0.5, size = 45, family = "caption"),
        panel.grid.major.x = element_line(linetype = "dotted"),
        panel.grid.major.y = element_blank(),
        axis.text.x = element_markdown(size = 50, family = "caption"),
        axis.text.y = element_markdown(size = 50, family = "caption", lineheight = 0.1, hjust = 0.5), 
        legend.title = element_markdown(size = 65, face = "bold", color = "#0F8B8D", family = "subtitle"),
        legend.text = element_markdown(size = 65, family = "caption", face = "bold"),
        legend.position = c(0.12, 1),
        legend.spacing.x = unit(0.01, "cm"),
        legend.spacing.y = unit(0.01, "cm"))


ggsave("/Users/muhammetozkaraca/Desktop/plot.png", dpi = 720,  height = 6, width = 8.5)
  
