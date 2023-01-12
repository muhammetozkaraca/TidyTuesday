library(tidyverse)
library(ggplot2)
library(tidylog)
library(maps)
library(cowplot)
library(showtext)
library(monochromeR)
library(paletteer)
library(scico)
library(wesanderson)


freedom <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-22/freedom.csv')

freedom<- freedom %>% 
  mutate(country=case_when(country== "Bolivia (Plurinational State of)"~"Bolivia",
                           country== "CÃƒÂ´te dÃ¢â‚¬â„¢Ivoire"~"Ivory Coast",
                           country== "Cabo Verde"~"Cape Verde",
                           country== "United States of America"~"USA",
                           country== "United Kingdom of Great Britain and Northern Ireland"~"UK",
                           country== "Czechia"~"Czech Republic",
                           country== "Syrian Arab Republic"~"Syria",
                           country== "Venezuela (Bolivarian Republic of)"~"Venezuela",
                           country== "Viet Nam"~"Vietnam",
                           country== "Iran (Islamic Republic of)"~"Iran",
                           country== "Russian Federation"~"Russia",
                           country== "Republic of Moldova"~"Moldova",
                           country== "Democratic People's Republic of Korea"~"North Korea",
                           country== "Republic of Korea"~"South Korea",
                           country== "Congo"~"Republic of Congo",
                           country== "Micronesia (Federated States of)"~"Micronesia",
                           country== "Lao People's Democratic Republic"~"Laos",
                           country== "North Macedonia"~"Macedonia",
                           country== "Brunei Darussalam"~"Brunei",
                           country== "United Republic of Tanzania"~"Tanzania",
                           TRUE ~ country))
                           
                           
world_countries <- map_data("world") %>%
  as.data.frame() %>%
  count(region) %>%
  select(-n) 


freedom_countries <- freedom %>%
  as.data.frame() %>%
  count(country) %>%
  select(-n)

names(freedom_countries)[1] <- "region"
setdiff(world_countries, freedom_countries)


names(freedom)[1] <- "region"
map_world <- map_data("world")

merged <- left_join(map_world, freedom, by = c("region"))

merged_subsetted <- merged %>%
  filter(year %in% c(1995,2000,2005,2010,2015,2020))

font_add_google("Redressed", family = "title")
font_add_google("Playfair Display", family = "subtitle")
font_add_google("Libre Franklin", family = "caption")
showtext_auto()


plot <- ggplot(merged_subsetted, aes(long, lat, group = group, fill = Status)) +
  geom_polygon () +
  facet_wrap(vars(year)) +
  ggthemes::theme_map() +
  labs(title = "World Freedom Index",
       subtitle = "Visualisation of countries' freedom status (1995-2020)") +
       # caption = "Source: Freedom House | Muhammet Ozkaraca") +
  theme (plot.title = element_text(hjust = 0.5, family = "title", size = 20, face = "bold"),
         plot.subtitle = element_text(hjust = 0.5, family = "subtitle", size = 14),
         plot.background = element_rect(fill = "cornsilk", color = "cornsilk"),
         legend.box.background = element_rect(fill="cornsilk",color= "cornsilk"),
         legend.background = element_rect(fill="cornsilk",color="cornsilk"),
         legend.position = c(0.05, 0.01),
         legend.direction = "horizontal",
         legend.key.size = unit(0.15, 'cm'), 
         legend.key.height = unit(0.15, 'cm'), 
         legend.key.width = unit(0.15, 'cm'),
         strip.background = element_blank()) +
         # plot.caption = element_text(family = "caption")) +
  scale_fill_brewer(palette = "Dark2", labels = c("Free", "Not Free", "Partially Free"))
  


new <- ggdraw () +
  draw_plot (plot) +
  draw_label("Since the Cold War's end, many have anticipated a global expansion of political and civil liberties.Nevertheless, contrary to their expectations,\n the world has experienced a new wave of politics, resulting in what Fareed Zekeria dubbed a 'Illeberal Democracy.' Countries that follow\n this route have regular elections. They lack, however, other critical facets of democratic governance, including judicial independence and fair\n elections.As part of this week's # TidyTuesday challenge, this map demonstrates countries' freedom status using the Freedom House data.",
             x=0.68, fontfamily = "caption", y=0.05, color = "#736060", size=5)



ggsave("world-freedom4.png",limitsize = FALSE,
       height = 1500,  width = 2500, 
       units = "px", dpi = 320)
