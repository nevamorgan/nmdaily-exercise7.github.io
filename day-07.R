#Name: Neva Morgan
#Date: 2025-03-24
#Purpose: COVID Data - Daily Exercise 7

#Question 1: Make a faceted line plot (geom_line) of the 6** states with most cases.
#Your X axis should be the date and the y axis cases.**

library(ggplot2)
library(tidyverse)
url <- 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv'
covid <- read_csv(url)


covid |>
  filter(date == max(date)) |>
  group_by(state) |>
  summarise(cases = sum(cases, na.rm = TRUE)) |>
  ungroup() |>
  slice_max(cases, n = 6) |>
  pull(state) ->
  top_states



covid |>
  filter(state %in% top_states) |>
  group_by(state, date) |>
  summarise(cases = sum(cases, na.rm = TRUE)) |>
  ungroup() |>
  ggplot(aes(x = date, y = cases, color = state)) +
  geom_line(size = 2) +
  facet_wrap(~state) +
  ggthemes::theme_fivethirtyeight() +
  theme(legend.position = 'NA') +
  labs(title = "Cummulative Case Counts of the 6 States with the Most Recent Cases",
       x = "Date",
       y = "Cases",
       caption = "ESS330 Daily Exercise 07 Neva Morgan")


#Question 2:Make a column plot (geom_col) of daily total cases in the USA.
#Your X axis should be the date and the y axis cases.

covid |>
  group_by(date) |>
  summarize(cases = sum(cases)) |>
  ggplot(aes(x = date, y = cases)) +
  geom_col(fill = "navy", color = "navy", alpha = .25) +
  geom_line(color = "navy", size = 1) +
  ggthemes::theme_fivethirtyeight() +
  labs(title = "Daily COVID Cases in the USA",
       x = "Date",
       y = "Cases",
       caption = "ESS330 Daily Exercise 07 Neva Morgan")

