library(tidyverse)
library(showtext)
library(patchwork)
library(janitor)
library(glue)
library(ggtext)
library(gghighlight)
library(lubridate)
library(gt)
library(gtExtras)
library(gtable)
library(stringr)


font_add_google("Archivo", family = "title")
font_add_google("Nunito", family = "subtitle")
font_add_google("Martel", family = "axis")
font_add_google("Spartan", family = "caption")
font_add_google("Lora", family = "label")

font_add('fa-reg', 'fonts/Font Awesome 6 Free-Regular-400.otf')
font_add('fa-brands', 'fonts/Font Awesome 6 Brands-Regular-400.otf')
font_add('fa-solid', 'fonts/Font Awesome 6 Free-Solid-900.otf')

showtext_auto()
dat <- tidytuesdayR::tt_load(2023, week = 6)

big_tech_stock_prices <- dat$big_tech_stock_prices
big_tech_company_name <- dat$big_tech_companies


big_tech_stock_prices <- big_tech_stock_prices %>%
  left_join(big_tech_company_name, by = "stock_symbol") %>%
  mutate(company = case_when(company == "International Business Machines Corporation" ~ "IBM",
                             TRUE ~ company),
         company = gsub(' ', '\n',company))

plot <- ggplot() +
  geom_line(data = big_tech_stock_prices, aes(x = date, y = close, group = company)) +
  gghighlight(company %in% c("Apple Inc.", "Adobe Inc.", "Amazon.com, Inc.", "Salesforce, Inc.", "Cisco Systems, Inc.",
                             "Alphabet Inc.", "IBM", "Intel Corporation", "Meta Platforms, Inc.", "Microsoft Corporation",
                             "Netflix, Inc.", "NVIDIA Corporation", "Oracle Corporation", "Tesla, Inc."),
              use_direct_label = FALSE,
              unhighlighted_params = list(linewidth = 0.1, colour = alpha("grey20", 0.3))) +
  geom_area(aes(date, close, colour = stock_symbol, fill = stock_symbol), big_tech_stock_prices, alpha = 0.2, size = 0.5) +
  geom_text(data = big_tech_stock_prices, aes(label = company, family = "label", x = lubridate::ymd("2014-01-01"), y = 500, color = company), size = 20, lineheight = .1) +
  facet_wrap(~company) +
  theme_minimal() +
  labs(title = "Stock Market Values for BigTech Companies (2010-2022)",
       subtitle = "The plot demonstrates the stock market value for 14 BigTech companies. Note that the values<br>displays the closed prices for each days.",
       y = "US Dollar - $",
       x = "",
       caption = "Source: Yahoo Finance by Kaggle | Plot: <span style='font-family:fa-brands'>&#xf09b;</span> muhammetozkaraca <span style='font-family:fa-brands'>&#xf099;</span> muhammetozkrca | #TidyTuesday-Week 6") +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.title.y = element_markdown(family = "axis", size = 55, linewidth = 0.2),
        axis.title.x = element_markdown(family = "axis", size = 55, linewidth = 0.2),
        axis.text.y = element_markdown(family = "axis", size = 35),
        axis.text.x = element_markdown(family = "axis", hjust = 0.43, size = 35),
        plot.title = element_markdown(family = "title", size = 95, hjust = 0.5, lineheight = 0.15, linewidth = 0.1),
        plot.subtitle = element_markdown(family = "subtitle", size = 75, hjust = 0.5, lineheight = 0.15, linewidth = 0.1),
        plot.caption = element_markdown(family = "title", size = 45, lineheight = 0.15, linewidth = 0.1, hjust = 0.5),
        plot.background = element_rect(fill = "white", color = "white"))

ggsave("/Users/muhammetozkaraca/Desktop/stock-prices-line-plot.png", height = 6,  width = 7.5, dpi = 720)
  









