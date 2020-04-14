library(httr)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(plotly)

raw_data <- GET('https://coronavirus-tracker-api.herokuapp.com/all') %>% .$content %>% rawToChar() %>% fromJSON(flatten = T)

prepare_data <- function(raw, type){
  tidy_data <- raw[[type]][['locations']] %>% select(country, contains('history')) %>% 
    as_tibble() %>% mutate_at(2:ncol(.), as.integer) %>%
    group_by(country) %>% summarise_all(.funs = sum) %>%
    rename_all(funs(gsub('history.', '', make.names(.)))) %>%
    pivot_longer(cols = colnames(.)[-1])
}

confirmed_cases <- prepare_data(raw_data, 'confirmed') %>% rename(date = name, 'confirmed' = value)
deaths <- prepare_data(raw_data, 'deaths') %>% rename(date = name, 'deaths' = value)
recovered <- prepare_data(raw_data, 'recovered') %>% rename(date = name, 'recovered' = value)

cov_complete_data <- left_join(confirmed_cases, deaths, by = c('country', 'date')) %>%
  left_join(recovered, by = c('country', 'date')) %>%
  mutate(date = mdy(date),
         active = confirmed - deaths - recovered) %>%
  group_by(country) %>% arrange(date) %>% mutate(confirmed_diff = confirmed - lag(confirmed, default = confirmed[1]),
                                                 deaths_diff = deaths - lag(deaths, default = deaths[1]))

options(scipen = 10000)

plotly::ggplotly(ggplot(cov_complete_data, aes(x = date)) +
  geom_line(aes(y = active, colour = country)))
