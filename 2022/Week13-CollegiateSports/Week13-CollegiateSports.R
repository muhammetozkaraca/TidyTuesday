library(tidyverse)
library(scales)
library(ggtext)
library(packcircles)
library(patchwork)
library(paletteer)
library(showtext)
library(cowplot)


tuesdata <- tidytuesdayR::tt_load(2022, week = 13)
sports <- tuesdata$sports

# Making Circle Plot
sports_expenditures <- sports %>%
  group_by(sports) %>%
  summarize(expenditure = sum(total_exp_menwomen, na.rm = TRUE)) %>%
  mutate(id = row(.),
         rounded = round(expenditure/1000000000, 2),
         label = paste0(sports,"\n",rounded," B USD"))


packing <- circleProgressiveLayout(sports_expenditures$expenditure, sizetype='area')
sports_expenditures <- cbind(sports_expenditures, packing)
dat.gg <- circleLayoutVertices(packing, npoints=50)
dat.gg$value <- rep(sports_expenditures$expenditure, each=51)

font_add_google("Libre Caslon Display", family = "title")
font_add_google("Playfair Display", family = "subtitle")
font_add_google("PT Serif", family = "caption")


font_add('fa-reg', 'fonts/Font Awesome 6 Free-Regular-400.otf')
font_add('fa-brands', 'fonts/Font Awesome 6 Brands-Regular-400.otf')
font_add('fa-solid', 'fonts/Font Awesome 6 Free-Solid-900.otf')

showtext_auto()


plot <- ggplot() + 
  geom_polygon(data = dat.gg, aes(x, y, group = id, fill= value), colour = "black", alpha = 0.6) +
  paletteer::scale_fill_paletteer_c("scico::lisbon") +
  # for discrete values; paletteer::scale_fill_paletteer_d("rtist::vangogh") or scale_color_paletteer_d("nord::aurora")
  geom_text(data = sports_expenditures, aes(x, y, size=expenditure, label = label)) +
  # scale_size_continuous(range = c(1,4)) +
  theme_void() + 
  coord_equal() +
  labs(title = "What Sports Do American Colleges Spend More On?",
       subtitle = "Between <span style = 'font-size:21pt;color:#005A9C;'>2015</span> and <span style = 'font-size:21pt;color:#005A9C;'>2019</span>, American colleges spent <span style = 'color:#E69F00;'>more than 63 Billion dollars</span> on different collegiate sports. <br>
       This visualisation illustrates the comparison of the total amount of expenses for each branch",
       caption = "Source: Equity in Athletics Data Analysis | Plot: <span style='font-family:fa-brands'>&#xf09b;</span> muhammetozkaraca <span style='font-family:fa-brands'>&#xf099;</span> muhammetozkrca | #TidyTuesday-Week 13") +
  theme(
    plot.background = element_rect(fill = "white",colour = NA),
    panel.background = element_rect(fill = 'white', colour = 'white'),
    panel.border = element_blank(),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, family = "title", size = 20),
    plot.subtitle = element_markdown(hjust = 0.5, family = "subtitle", size = 14), # note that to color numbers differently, I used element_markdown not element_text function
    plot.caption = element_markdown(hjust = 0.5, family = "caption", size = 11))


ggsave("circle-plot.png", height = 7, width = 12)






