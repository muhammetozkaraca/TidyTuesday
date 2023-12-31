library(tidyverse)
library(showtext)
library(showtextdb)
library(ggthemes)
library(ggtext)
library(magick)
library(ggimage)
library(cropcircles)
library(cowplot)
library(patchwork)


font_add_google("Lora", bold.wt = 700, family = "plottitle")
font_add_google("Roboto Slab", family = "title") 
font_add_google("Ubuntu", family = "caption")

font_add('fa-reg', 'fonts/Font Awesome 6 Free-Regular-400.otf')
font_add('fa-brands', 'fonts/Font Awesome 6 Brands-Regular-400.otf')
font_add('fa-solid', 'fonts/Font Awesome 6 Free-Solid-900.otf')
showtext_auto()

bob_ross <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-21/bob_ross.csv')
bob_ross_top <- bob_ross %>%
  mutate(col_num_n = str_count(colors, ",") + 1) %>%
  arrange(desc(col_num_n)) %>%
  slice(1:10) %>%
  mutate(image = map(img_src, image_read),
         img_circle = circle_crop(img_src, border_size = 12, border_colour = "white"),
         painting_title = gsub(" ", "<br>", painting_title),
         painting_title = case_when(painting_title == "Lake<br>in<br>the<br>Valley" ~ "Lake<br>in the<br>Valley",
                                    painting_title == "Waterfall<br>in<br>the<br>Woods" ~ "Waterfall<br>in the<br>Woods",
                                    .default = as.character(painting_title)))


plot <- ggplot() +
  geom_image(data = bob_ross_top, aes(x = 1, y = 1, image = img_circle), size = 0.8) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(caption = "Source: Bob Ross Paintings | Plot:<span style='font-family:fa-brands'>&#xf09b; </span>muhammetozkaraca <span style='font-family:fa-brands'>&#xf099;</span> @muhammetozkrca | #TidyTuesday-Week 8") +
  theme_void() +
  facet_wrap(~painting_title, ncol = 4, strip.position = "right") +
  theme(# axis.text = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        strip.background = element_blank(),
        strip.text.y.right = element_markdown(hjust = 0, angle = 0, size = 60, family = "title", face = "bold", lineheight = 0.1),
        panel.spacing.y = unit(1, "cm"),
        panel.spacing.x = unit(0.5, "cm"),
        plot.background = element_rect(fill = "cornsilk", color = "cornsilk"),
        panel.background = element_rect(fill = "cornsilk", color = "cornsilk"),
        plot.caption = element_markdown(linewidth = 0.02, hjust = 0.5, size = 45, family = "caption"))


title_plot <- ggplot() +
  geom_text(aes(x = 2, y = 25, label = "Bob Ross\nPainting"), 
            size = 60, family = "plottitle", fontface = "bold", color = "black", lineheight = 0.1) +
  geom_text(aes(x = 0, y = 20, label = "This plot demonstrates\n10 most colorful\npaintings in Bob\nRoss's show."), 
            size = 30, family = "title", color = "black", lineheight = 0.1) +
  scale_y_continuous(expand = c(0, 12)) +
  scale_x_continuous(expand = c(0, 12)) +
  theme_void() +
  theme(plot.background = element_rect(fill = "cornsilk", color = "cornsilk"),
        panel.background = element_rect(fill = "cornsilk", color = "cornsilk"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        # axis.text = element_blank(),
        axis.ticks = element_blank())

final_plot <- title_plot +
  plot +
  plot_layout(widths = c(1, 5)) +
  theme(plot.background = element_rect(fill = "cornsilk", color = "cornsilk"),
        panel.background = element_rect(fill = "cornsilk", color = "cornsilk"))

ggsave("/Users/muhammetozkaraca/Desktop/plot.png", dpi = 720,  height = 6, width = 12)




