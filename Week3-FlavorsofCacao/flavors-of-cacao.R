library(tidyverse)
library(ggplot2)
library(gt)
library(dplyr)
library(gtExtras)
library(data.table)
library(formattable)
library(tidyr)
library(readr)
library(skimr)
library(sparkline)

customGreen0 = "#DeF7E9"
customGreen = "#71CA97"
customRed = "#ff7f7f"

data <- read_csv("flavors_of_cacao.csv")
head(data)
colnames(data)
summary(data)


names(data)[1] <- "company"
names(data)[2] <- "specific_origin"
names(data)[4] <- "review_date"
names(data)[5] <- "cocoa_percent"
names(data)[6] <- "company_location"
names(data)[7] <- "rating"
names(data)[8] <- "bean_type"
names(data)[9] <- "bean_origin"


formattable(data)


# Adding Color Mapping
data$cocoa_percent <- parse_number(data$cocoa_percent)

data <- data %>%
  mutate(average_rating = mean(rating)) %>%
  group_by(company) %>%
  mutate(average_rating_by_company = mean(rating))

head(data)

formattable(data, align = c("c","c","c","c","c", "c", "c", "c", "c"), 
            list("company" = formatter("span", style = ~ style(color = "grey",font.weight = "bold")), 
                 "rating"= color_tile("transparent", "lightpink"),
                 "cocoa_percent"= color_tile(customGreen, customGreen0),
                 "average_rating" = color_bar("lightblue")
            ))

improvement_formatter <- formatter("span", 
                                   style = x ~ style(
                                     font.weight = "bold", 
                                     color = ifelse(data$average_rating_by_company > data$average_rating, customGreen, 
                                                    ifelse(data$average_rating_by_company < data$average_rating, customRed, "black"))), 
                                   x ~ icontext(ifelse(data$average_rating_by_company > data$average_rating, "arrow-up", "arrow-down"), x))


data$average_rating_by_company <- format(round(data$average_rating_by_company, 2), nsmall = 2)
data$average_rating <- format(round(data$average_rating, 2), nsmall = 2)
data$rating <- format(round(data$rating, 2), nsmall = 2)


formattable(data, align = c("c","c","c","c","c", "c", "c", "c", "c"), 
            list("company" = formatter("span", style = ~ style(color = "grey",font.weight = "bold")), 
                 "rating"= color_tile("transparent", "lightpink"),
                 "cocoa_percent"= color_tile(customGreen, customGreen0),
                 "average_rating" = color_bar("lightblue"),
                 "average_rating_by_company" = improvement_formatter
            ))


data_company <- data %>%
  select(company, average_rating, average_rating_by_company) %>%
  group_by(company, average_rating_by_company) %>%
  filter(n()>10) %>%
  distinct(company, .keep_all = T)

improvement_formatter_new <- formatter("span", 
                                   style = x ~ style(
                                     font.weight = "bold", 
                                     color = ifelse(data_company$average_rating_by_company > data_company$average_rating, customGreen, 
                                                    ifelse(data_company$average_rating_by_company < data_company$average_rating, customRed, "black"))), 
                                   x ~ icontext(ifelse(data_company$average_rating_by_company > data_company$average_rating, "arrow-up", "arrow-down"), x))

formattable(data_company, align = c("c","c","c"), 
            list("company" = formatter("span", style = ~ style(color = "grey",font.weight = "bold")),
                 "average_rating" = color_bar("lightblue"),
                 "average_rating_by_company" = improvement_formatter_new
            ))


data_box_plot <- data %>%
  group_by(bean_origin) %>%
  filter(n() > 7) %>% 
  mutate(origin_cacao_average = mean(cocoa_percent)) %>%
  ggplot() +
  geom_boxplot(aes(x = bean_origin, y = cocoa_percent, fill = origin_cacao_average)) +
  scale_fill_continuous(low = '#ffffcc', high = '#fc4e2a', name = "Average Cacao Percent") +
  theme_minimal() +
  coord_flip() +
  labs(x = 'Bean Origin', y = 'Cacao Percent')


data[data$bean_type== " "] <- NA            # Replace particular value with NA
library(stringr)
data$bean_type <- str_replace(data$bean_type, " ", "NA")




abab <- data %>%
  str_replace(" ", "NA") %>%
  drop_na(bean_type) %>%
  group_by(bean_type) %>%
  filter(n()>7) %>%
  mutate(average_rating_by_type = mean(rating)) %>%
  ggplot() +
  geom_boxplot(aes(x = bean_type, y = average_rating_by_type, fill = average_rating)) +
  theme_minimal() +
  coord_flip() +
  labs(x = 'Bean Type', y = 'Ratings')


colnames(data_box_plot)

data %>%
  group_by(bean_origin) %>% 
  filter(n() > 10) %>% 
  mutate(avg = mean(Rating)) %>%
  ggplot() + 
  geom_boxplot(aes(reorder(bean_origin, avg), Rating, fill = avg)) + 
  scale_fill_continuous(low = '#ffffcc', high = '#fc4e2a', name = "Average rating") + 
  coord_flip() + 
  theme_minimal() + 
  labs(x = 'Bean Origin', y = 'Rating') +
  expand_limits(y = c(0,5))


data %>%
  group_by(company_location) %>% 
  mutate(avg = mean(Rating)) %>%
  ggplot() + 
  geom_boxplot(aes(reorder(company_location, avg), Rating, fill = avg)) + 
  scale_fill_continuous(low = '#ffffcc', high = '#fc4e2a', name = "Average rating") + 
  coord_flip() + 
  theme_minimal() + 
  labs(x = 'Company Location', y = 'Rating') +
  expand_limits(y = c(0,5))


#Set a few color variables to make our table more visually appealing

customGreen0 = "#DeF7E9"

customGreen = "#71CA97"

customRed = "#ff7f7f"


choco<- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-18/chocolate.csv')

# Get company_manufacturer with more than 15 reviews
df1 = choco %>% count(company_manufacturer, sort=T) %>% filter(n>15)

# Table data
df2 = choco %>% 
  filter(company_manufacturer %in% df1$company_manufacturer)  %>% 
  group_by(company_manufacturer) %>%
  mutate(avg=mean(rating)) %>%
  ungroup() %>%
  mutate(rank = dense_rank(desc(avg))) %>%
  arrange(rank)

rate_list <- split(df2$rating, df2$rank)
rate_rng <- range(df2$rating)

# Inline boxplot function
# Citation: Mock (2020, Oct. 31). The Mockup Blog: Embedding custom HTML in gt tables. Retrieved from https://themockup.blog/posts/2020-10-31-embedding-custom-features-in-gt-tables/
gt_plot <- function(table_data, column, plot_data, plot_fun, ...){
  text_transform(
    table_data,
    # note the use of {{}} here - this is tidy eval
    # that allows you to indicate specific columns
    locations = cells_body(columns = vars({{column}})),
    fn = function(x){
      plot <- map(plot_data, plot_fun, width = 300, height = 70, same_lim = TRUE, ...)
      plot_svg <- map(plot, "svg_text")
      map(plot_svg, gt::html)
    }
  )
}

# {gt} table
df2 %>%
  mutate(company_manufacturer==case_when(company_manufacturer=="Smooth Chocolator, The" ~ "The Smooth Chocolator", 
                                         TRUE~company_manufacturer)) %>%
  group_by(company_location,company_manufacturer) %>%
  summarise(n=n(),
            average = round(mean(rating),2),
            min=min(rating),
            median = round(median(rating),2),
            max=max(rating),
            range= max-min,
            histogram=list(rating),
            .groups="drop") %>%
  arrange(desc(average)) %>%
  mutate(boxplot ="",
         n2 = n, 
         #rank = dense_rank(desc(avg))
  ) %>%
  select(company_location, company_manufacturer, n, n2, average, histogram, min, median, max, range, boxplot) %>%
  gt() %>%
  gt_theme_538() %>%
  gt_sparkline(
    histogram,
    type = "histogram",
    line_color = "#66462c",
    fill_color = "#66462c",
    bw = .25,
    same_limit = TRUE
  ) %>%
  gt_plot(
    column = boxplot,  # column to create plot in 
    plot_data = rate_list, # external data to reference
    plot_fun = spec_boxplot,  # which plot fun
    lim = rate_rng, # range applied
  ) %>%
  gt_plt_bar(column=n2, 
             color="#82a6b1",
             width=30) %>%
  gt_merge_stack(company_manufacturer, company_location, colors=c("#38160d","grey")) %>%
  gt_color_rows(columns = c("average","range"),
                palette = "ggsci::brown_material") %>%
  cols_align(columns = c("histogram", "boxplot", "median"),
             align="center") %>%
  cols_label(company_manufacturer = html("Manufacturer"),
             n=html(""),
             n2=html("N reviewed")) %>%
  tab_spanner(label="Rating", 
              columns=c(average:boxplot)) %>%
  tab_header(title=md("<span style='color:#411d13'>Ratings of Plain Dark Chocolate Bar</span>"),
             subtitle=md("Summary table of ratings (between 1 to 5) by 22 manufacturers with more than 15 reviews, according to *Flavors of Cacao*.")) %>%
  tab_source_note(source_note = gt::html("<br>#TidyTuesday Week 3  |  Data source: Flavors of Cacao, by way of Georgios and Kelsey  |  Inline boxplot adapted from Tom Mock")) %>%
  # Adjust sub-title font
  tab_style(
    style = list(
      cell_text(
        weight="lighter"
      )
    ),
    locations = list(
      cells_title(groups = "subtitle")
    )
  ) 