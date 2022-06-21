library(tidyverse)
library(maps)
library(sf)
data(world.cities)


slave_routes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/slave_routes.csv')
world_countries <- map_data("world") %>%
  as.data.frame()


world.cities$name[5393] <- "Bristol (RI)"
world.cities$name[34630] <- "Seville"
world.cities$name[11174] <- "Falmouth (Eng.)"
world.cities$name[29665] <- "Portsmouth (NH)"
world.cities$name[32161] <- "Saint John (Antigua)"
world.cities$name[21165] <- "Liverpool-UK"
world.cities$name[21344] <- "London-UK"
world.cities$name[3386] <- "Barcelona-Spain"
world.cities$name[20145] <- "Lancaster-UK"
world.cities$name[20145] <- "Hamilton (Bermuda)"
world.cities$name[11173] <- "Falmouth (Jam.)"
world.cities$name[25889] <- "Newcastle (Nevis)"


merged <- slave_routes %>%
  mutate(port_origin = gsub(", port unspecified", "", port_origin),
         port_arrival = gsub(", port unspecified", "", port_arrival)) %>%
  mutate(port_origin = case_when(
    port_origin == "Havana" ~ "Havanna",
    port_origin == "Maranhao" ~ "Sao Mateus do Maranhao",
    port_origin == "Zeeland" ~ "Zeewolde",
    port_origin == "Kobenhavn" ~ "Copenhagen",
    port_origin == "Gustavia, St. Barthelemy" ~ "Gustavia",
    port_origin == "La Coruna" ~ "A Coruna",
    port_origin == "Cape Coast Castle" ~ "Cape Coast",
    port_origin == "Cowes" ~ "Cowes-Northwood",
    port_origin == "Poulton" ~ "Poulton-le-Fylde",
    port_origin == "Rio de Janeiro province" ~ "Rio de Janeiro",
    port_origin == "Cape Coast Castle" ~ "Cape Coast",
    port_origin == "Dunkerque" ~ "Dunkirk",
    port_origin == "Les Sables" ~ "Les Sables-d'Olonne",
    port_origin == "Liverpool" ~ "Liverpool-UK",
    port_origin == "London" ~ "London-UK",
    port_origin == "Barcelona" ~ "Barcelona-Spain",
    port_origin == "Lancaster" ~ "Lancaster-UK",
    TRUE ~ port_origin)) %>%
  mutate(port_arrival = case_when(
    port_arrival == "Havana" ~ "Havanna",
    port_arrival == "Maranhao" ~ "Sao Mateus do Maranhao",
    port_arrival == "Zeeland" ~ "Zeewolde",
    port_arrival == "Kobenhavn" ~ "Copenhagen",
    port_arrival == "Gustavia, St. Barthelemy" ~ "Gustavia",
    port_arrival == "La Coruna" ~ "A Coruna",
    port_arrival == "Cape Coast Castle" ~ "Cape Coast",
    port_arrival == "Cowes" ~ "Cowes-Northwood",
    port_arrival == "Poulton" ~ "Poulton-le-Fylde",
    port_arrival == "Rio de Janeiro province" ~ "Rio de Janeiro",
    port_arrival == "Cape Coast Castle" ~ "Cape Coast",
    port_arrival == "Dunkerque" ~ "Dunkirk",
    port_arrival == "Les Sables" ~ "Les Sables-d'Olonne",
    port_arrival == "Liverpool" ~ "Liverpool-UK",
    port_arrival == "London" ~ "London-UK",
    port_arrival == "Barcelona" ~ "Barcelona-Spain",
    port_arrival == "Lancaster" ~ "Lancaster-UK",
    TRUE ~ port_arrival)) %>%
  subset(!is.na(port_origin)) %>%
  subset(!is.na(port_arrival)) %>%
  left_join(world.cities, by = c("port_origin" = "name")) %>%
  rename("origin_lat" = lat,
         "origin_long" = long) %>%
  left_join(world.cities, by = c("port_arrival" = "name")) %>%
  rename("arrival_lat" = lat,
         "arrival_long" = long,
         "departure_country" = country.etc.x,
         "arrival_country" = country.etc.y) %>%
  select(year_arrival, n_slaves_arrived, ship_name, port_origin, departure_country, port_arrival,
         arrival_country, origin_lat, origin_long, arrival_lat, arrival_long) %>%
  na.omit()


merged_17 <- merged %>%
  filter(year_arrival>1599 & year_arrival<1699)

all_pairs <- merged %>%
  filter(year_arrival>1599 & year_arrival<1699) %>%
  rename("lat1" = origin_lat,
         "lat2" = arrival_lat,
         "long1" = origin_long,
         "long2" = arrival_long)


ggplot(data = world_countries, aes(x = long, y = lat)) +
  geom_path(aes(group=group), size=0.1, alpha = 0.5) +
  geom_point(data = merged_17, aes(x = origin_long, y = origin_lat), alpha = 0.5) +
  geom_point(data = merged_17, aes(x = arrival_long, y = arrival_lat), alpha = 0.1) +
  theme_void()


all_pairs <- cbind(t(combn(merged_17$long, 2)), t(combn(data$lat, 2))) %>% as.data.frame()
colnames(all_pairs) <- c("long1","long2","lat1","lat2")

par(mar=c(0,0,0,0))
map('world',col="#f2f2f2", fill=TRUE, bg="white", lwd=0.05,mar=rep(0,4),border=0, ylim=c(-80,80) )
for(i in 1:nrow(all_pairs)){
  plot_my_connection(all_pairs$long1[i], all_pairs$lat1[i], all_pairs$long2[i], all_pairs$lat2[i], col="skyblue", lwd=1)
}
points(x=data$long, y=data$lat, col="slateblue", cex=2, pch=20)
text(rownames(data), x=data$long, y=data$lat,  col="slateblue", cex=1, pos=4)



