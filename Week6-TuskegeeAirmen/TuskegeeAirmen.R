library(tidyverse)
library(stringr)
library(ggplot2)
library(ggtext)
library(maps)
library(viridis)
library(showtext)
library(monochromeR)
library(colorblindr)
library(paletteer)



tuesdata <- tidytuesdayR::tt_load(2022, week = 6)
airmen <- tuesdata$airmen

us_states <- map_data("state")
us_states$region <- str_to_title(us_states$region) 


us_states$state <- state.abb[match(us_states$region,state.name)]


airmen$state[airmen$state == "Haiti"] <- "HT"               
airmen$state[airmen$state == "In"] <- "IN"               


subsetted <- airmen %>%
  group_by(state) %>%
  summarise(count = n()) %>%
  select(state, count) %>%
  slice(-c(39)) %>%
  na.omit
  


merged <- left_join(us_states, subsetted, by = c("state"))
merged$count[is.na(merged$count)] <- 0


font_add_google("Dancing Script", family = "title")
font_add_google("Inconsolata", family = "subtitle")
font_add_google("Akaya Telivigala", family = "caption")
showtext_auto()


library(scico)

scico(6, palette = 'davos')

monochromeR::generate_palette("#FFFFFF",
                              modification = "go_darker", 
                              n_colours = 6, view_palette = T)

monochromeR::generate_palette("#2A5E97", modification = "blend", 
                              blend_colour = "#D7D8A0", 
                              n_colours = 5, view_palette = T)

# colorblindr::cvd_grid(



merged[rev(order(merged$count)),]


plot <- ggplot(merged, aes(long, lat, group = group, fill = count)) +
  geom_polygon() +
  theme_void() +
  labs(title = "TUSKEGEE AIRMEN FIGHTERS' HOMETOWNS", 
       subtitle = "<span style = 'font-size:12pt'>The Tuskegee Airmen were a group of African American military pilots who fought during World War II. This map visualizes the distribution of their hometown states. 
       In total, this data represents <span style='color:#B22222'>994</span> pilots' hometowns. Please note that the original dataset includes <span style='color:#B22222'>12</span> more pilots 
       for whom the data about their hometown was not included in this visualisation, as it was missing.",
       caption = "#TidyTuesday Challenge | Muhammet Ozkaraca") +
  scale_fill_gradientn(
    "Number of Pilots",
    colours = c("#ADADAD", "#2A5E97", "#4C7698", "#6F8E9A", "#91A79C", "#B4BF9E"),
    breaks=c(0,7,13,37,80,100),
    guide = guide_legend(
      title.position = "top",
      label.position = "bottom",
      title.hjust = .5,
      barwidth = 23, 
      barheight = .5,
      nrow=1,
      keyheight = unit(3, units = "mm"),
      keywidth=unit(12, units = "mm"))) +
  theme(
    plot.title = element_text(hjust = 0.3,
                              family = "title",
                              size = 20,
                              face = "bold"),
    plot.subtitle = element_textbox_simple(
      family = "subtitle",
      size = 11,
      lineheight = 0.8,
      padding = margin(5.5, 5.5, 5.5, 5.5),
      margin = margin(-4, -6, -12, -1),
      fill = "#f5f5f2"
    ),
    # plot.subtitle = element_markdown(valign = TRUE, hjust = 0, halign = TRUE, linewidth = 1),
    # plot.subtitle = element_textbox_simple(size = 12, lineheight = 1, padding = margin(0, 0, 5, 0),
    #                                       family = "subtitle"),
    plot.caption = element_text(family = "caption"),
    legend.position = "bottom",
    plot.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.background = element_rect(fill = "#f5f5f2", color = NA),
    legend.background = element_rect(fill = "#f5f5f2", color = NA)
  ) 


plot + annotate(geom = "text", x = -70, y = 33,
           color = "#4C7698", label = "Illinois is \n the top \n state with \n 102 pilots' \n hometown", 
           family = "subtitle", fontface = "bold") +
  geom_curve(aes(x = -90, y = 41, xend = -70, yend = 39),
             curvature = -0.3,
             color = "darkgoldenrod1", size = 0.5,
             alpha = 0.7,
             arrow = arrow(length = unit(0.03, "npc"),
                           type = "closed")) 

