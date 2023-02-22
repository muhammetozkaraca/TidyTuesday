library(tidyverse)
library(gt)
library(gtExtras)
library(showtext)
library(showtextdb)
library(lubridate)

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
big_tech_companies <- dat$big_tech_companies

big_tech_stock_prices <- big_tech_stock_prices %>%
  left_join(big_tech_companies, by = "stock_symbol") %>%
  mutate(company = case_when(company == "International Business Machines Corporation" ~ "IBM",
                             TRUE ~ company))

data <- big_tech_stock_prices %>%
  group_by(company) %>%
  mutate(closing_prices = list(close)) %>%
  subset(date > make_date("2021", "12", "31") & date < make_date("2023", "01", "01")) %>%
  mutate(prices_last_year = list(close)) %>%
  subset(date > make_date("2022", "07", "01") & date < make_date("2023", "01", "01")) %>%
  mutate(prices_last_six_month = list(close)) %>%
  subset(date > make_date("2022", "11", "30") & date < make_date("2023", "01", "01")) %>%
  mutate(prices_last_month = list(close)) %>%
  select(company, stock_symbol, closing_prices,  prices_last_month, prices_last_six_month, prices_last_year) %>%
  unique()

base_url = "https://raw.githubusercontent.com/tashapiro/TidyTuesday/master/2023/W6/logos/"

gt <- data %>%
  mutate(stock_symbol1 = stock_symbol,
         stock_symbol = paste0("SYMBL: ",stock_symbol),
         logo = glue::glue('{base_url}{stock_symbol1}.png')) %>%
  ungroup() %>%
  select(logo, company, stock_symbol, closing_prices, prices_last_month, prices_last_six_month, prices_last_year) %>%
  gt() %>%
  gtExtras::gt_img_rows(columns = 'logo', height = 50) %>%
  gt_plt_sparkline(closing_prices) %>%
  gt_plt_sparkline(prices_last_year, type = "shaded", label = FALSE, palette = c("lightgrey", rep("transparent", 3), "#B35A20")) %>%
  gt_plt_sparkline(prices_last_six_month, type = "shaded", label = FALSE, palette = c("lightgrey", rep("transparent", 3), "#BFD5C9")) %>%
  gt_plt_sparkline(prices_last_month, type = "shaded", label = FALSE, palette = c("lightgrey", rep("transparent", 3), "#006373")) %>%
  cols_label(company = "COMPANY",
             prices_last_year = "1Y",
             prices_last_six_month = "6M",
             prices_last_month = "1M",
             closing_prices = "Closing Prices over Time\n(2020-2022)",
             logo = "") %>%
  cols_merge(columns = vars(company, stock_symbol)) %>%
  text_transform(locations = cells_body(columns = vars(company, stock_symbol)),
                 fn = function(x){
                   company <- word(x, 1)
                   stock <- word(x, -1)
                   glue::glue("<div><span style='font-weight:bold;font-variant:small-caps;font-size:14px'>{company}</div>
        <div><span style ='font-weight:bold;color:grey;font-size:10px'>{stock}</span></div>")}) %>%
  cols_width(company ~ px(145),
             closing_prices ~ px(65)) %>%
  tab_header(title = "BIG TECH FIRMS' STOCK PRICES TRENDS",
             subtitle = "Daily stock prices trend for Big Tech Firms in 2022") %>%
  tab_spanner(label = "Periods", columns = c(prices_last_year,prices_last_six_month, prices_last_month)) %>%
  tab_style(locations = cells_title('title'),
            style = cell_text(align = 'left', font = 'Lora', size = 'xx-large', weight = 600)) %>% 
  tab_style(locations = cells_title('subtitle'), 
            style = cell_text(align = 'left', font = 'Merriweather', size = 'x-large')) %>%
  tab_style(locations = cells_column_labels(columns = c("company")),
            style = cell_text(align = 'left', font = 'Archivo', size = 'large', weight = 'bold')) %>%
  tab_style(locations = cells_column_labels(columns = c("closing_prices", "prices_last_year", "prices_last_six_month", "prices_last_month")),
            style = cell_text(align = 'center', font = 'Archivo', size = 'large', weight = 'bold')) %>%
  tab_style(locations = cells_column_labels(columns = c("logo", "company", "closing_prices", "prices_last_year", "prices_last_six_month", "prices_last_month")),
            style = cell_borders(sides = "bottom", color = "black", weight = px(3))) %>%
  tab_style(locations =  cells_column_spanners(), style = cell_text(align = 'center', weight = 'bold', font = 'Nunito')) %>%
  tab_options(heading.align = "left",
              table.border.top.color = "white",
              table.border.bottom.color = "black",
              table.border.bottom.width = px(25)) %>%
  tab_footnote(footnote = glue::glue('Source: Yahoo Finance by Kaggle | Table: {fontawesome::fa("twitter", fill = "#104e8b")}@muhammetozkaraca {fontawesome::fa("github", fill = "black")}@muhammetozkaraca | #TidyTuesday-Week 6') |> html(),
                   placement = 'left') %>%
  opt_css(
    css = '.gt_footnote {
      text-align: left; 
      padding-top: 5px;
      padding-bottom: 5px;
      font-family: Merriweather;
    }')
                 
                 
                
gtsave(gt, "/Users/muhammetozkaraca/Desktop/tab_3.png")
gtsave(gt, "/Users/muhammetozkaraca/Desktop/tab1.png")

