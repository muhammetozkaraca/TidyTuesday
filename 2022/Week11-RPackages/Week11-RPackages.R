library(tidyverse)
library(showtext)
library(gt)
library(gtExtras)


font_add_google("Rock 3D", family = "title")
font_add_google("Libre Caslon Display", family = "subtitle")
font_add_google("Space Mono", family = "axis")
font_add_google("Spartan", family = "caption")

showtext_auto()

tuesdata <- tidytuesdayR::tt_load(2022, week = 11)
cran <- tuesdata$cran

cran <- cran %>%
  group_by(package) %>%
  summarise(update_count = n()) %>%
  arrange(desc(update_count)) %>%
  head(10)

gt_table <- gt(cran)

table <- cran %>%
  gt() %>%
  tab_header(
    title = html('<p style="font-family:title">Most Updated R Packages</p>'),
    subtitle = html('<p style="font-family:subtitle">The top ten most updated R Packages</p>')) %>%
  tab_source_note(
    source_note = html('<p style="font-family:caption">Source: Robert Flight | Making: @muhammetozkrca | TidyTuesday-Week 11</p>')
  )


gtsave(table, "table.png")

gt_tbl <- 
  gt_tbl %>%
  tab_source_note(
    source_note = "Source: The World Almanac and Book of Facts, 1975, page 406."
  ) %>%
  tab_source_note(
    source_note = md("Reference: McNeil, D. R. (1977) *Interactive Data Analysis*. Wiley.")
  )

# Show the gt table
gt_tbl






