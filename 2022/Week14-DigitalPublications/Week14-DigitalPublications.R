library(tidyverse)
library(showtext)
library(maps)
library(countrycode)
library(geofacet)


news_orgs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-05/news_orgs.csv')

url = "https://raw.githubusercontent.com/vincentarelbundock/countrycode/master/data/custom_dictionaries/us_states.csv"
state_dict = read.csv(url, stringsAsFactors=FALSE)
names(state_dict)[1] <- "region"
state_dict$region <- str_to_lower(state_dict$region)

us_states <- map_data("state") %>%
  left_join(state_dict, by = "region") %>%
  rename("state" = "abbreviation")


mapvis <- news_orgs %>%
  filter(country == "United States") %>%
  group_by(state, is_owner_founder) %>%
  mutate(count = n()) %>%
  select(publication_name, is_owner_founder, state, count) %>%
  left_join(us_states, by = "state") %>%
  ggplot()
  
  
  ggplot(aes(x = lat, y = long, group = group, fill = is_owner_founder)) +
  geom_col() +
  coord_flip() +
  facet_geo(~ state) +
  theme_bw()
  




subsetted <- news_orgs %>%
  select(publication_name, tax_status_current, budget_percent_editorial, 
         budget_percent_revenue_generation, budget_percent_product_technology, 
         budget_percent_administration) %>%
  pivot_longer(budget_percent_editorial:budget_percent_administration, 
               names_to = "budget_allocation", values_to = "percentage") %>%
  gsub('budget_percent_','', subsetted$budget_allocation)


subsetted$budget_allocation <- gsub('budget_percent_','', subsetted$budget_allocation)

subsetted %>%
  group_by(tax_status_current) %>%
  ggplot(aes(x=tax_status_current, y = percentage, fill = budget_allocation)) +
  geom_bar(position="stack", stat="identity") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))

