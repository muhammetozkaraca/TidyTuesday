library(ggplot2)
library(sf)
library(tidyverse)
library(showtext)
library(showtextdb)
library(ggtext)

sf_use_s2(FALSE)

font_add_google("Lora", bold.wt = 700, family = "plottitle")
font_add_google("Roboto Slab", family = "title") 
font_add_google("Ubuntu", family = "caption")

font_add('fa-reg', 'fonts/Font Awesome 6 Free-Regular-400.otf')
font_add('fa-brands', 'fonts/Font Awesome 6 Brands-Regular-400.otf')
font_add('fa-solid', 'fonts/Font Awesome 6 Free-Solid-900.otf')
showtext_auto()

us_place_names <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-27/us_place_names.csv')
us_place_history <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-27/us_place_history.csv')

us_place_names_tidied <- us_place_names %>%
  drop_na(prim_long_dec, prim_lat_dec) %>%
  # filter(prim_lat_dec < 50, prim_lat_dec > 15, prim_long_dec < -68, prim_long_dec > -140) %>% 
  # st_as_sf(coords = c("prim_long_dec", "prim_lat_dec"), crs=4326) %>%
  st_as_sf(coords = c("prim_long_dec", "prim_lat_dec"), crs=4326) %>%
  st_crop(xmin = -140, xmax = -68, ymin = 15, ymax = 50) %>%
  st_transform(crs = st_crs("+proj=laea +lat_0=45 +lon_0=-100")) 

lon <- c(-140, -64)
lat <- c(10, 51)

poly = data.frame(lon, lat) |>
  sf::st_as_sf(coords = c("lon", "lat"), crs = "EPSG:4326") |> 
  sf::st_bbox() |> 
  sf::st_as_sfc() |>
  sf::st_as_sf()

polc <- sf::st_make_grid(poly, n = c(20, 20), crs = "EPSG:4326") |>
  sf::st_as_sf()


usa <- maps::map("world", fill=TRUE, plot =FALSE) %>% # a different way to load world countries' geo data into the working environment
  st_as_sf() %>% 
  filter(ID %in% c("USA", "Canada")) %>%
  sf::st_transform(crs = "EPSG:4326") |>
  sf::st_crop(xmin = -140, xmax = -64, ymin = 10, ymax = 51)

usa_cropped <-  sf::st_intersection(usa, polc) |>
  dplyr::group_by(ID) |>
  dplyr::summarise() %>%
  sf::st_transform(crs = sf::st_crs("+proj=laea +lat_0=45 +lon_0=-100"))



lakes <- maps::map("lakes", fill=TRUE, plot =FALSE) %>%
  st_as_sf() %>%
  st_crop(xmin = -140, xmax = -55, ymin = 15, ymax = 40) %>%
  st_transform(crs = st_crs("+proj=laea +lat_0=45 +lon_0=-100")) 


plot <- ggplot() +
  geom_sf(data = usa_cropped, fill = "#1D7874") +
  geom_sf(data = usa %>% filter(ID == "USA"), fill = "#231709", color = "black") +
  geom_sf(data = lakes , fill = "lightblue", color = "transparent") +
  geom_sf(data = us_place_names_tidied, size = 0.0000001, color = "#FF8551", alpha = 0.1) +
  coord_sf(crs = st_crs("+proj=laea +lat_0=45 +lon_0=-100")) +
  scale_x_continuous(labels = function(x) paste0(x, "° W")) +
  scale_y_continuous(labels = function(y) paste0(y, "° N")) +
  labs(title = "US Populated Places", 
       caption = "Source: GNIS | Plot: <span style='font-family:fa-brands'>&#xf09b;</span> muhammetozkaraca <span style='font-family:fa-brands'>&#xf099;</span> @muhammetozkrca | #TidyTuesday-Week 26") +
  theme(plot.background = element_rect(fill = "lightblue", color = "lightblue"),
        panel.background = element_rect(fill = "lightblue", color = "lightblue"),
        plot.title = element_markdown(size = 100, hjust = 0.5, family = "title", face = "bold", linewidth = 0.3),
        plot.subtitle = element_markdown(size = 65, hjust = 0, family = "title", lineheight = 0.2),
        plot.caption = element_markdown(size = 45, hjust = 0.5, family = "caption", linewidth = 0.01),
        axis.ticks = element_blank(),
        axis.text.x = element_markdown(size = 30, family = "caption", face = "bold"),
        axis.text.y = element_markdown(size = 30, family = "caption", face = "bold"))

ggsave("/Users/muhammetozkaraca/Desktop/plot.png", dpi = 720,  height = 6, width = 8)









