start_confirm_cases <- 100

cov_significant_confirmed_cases <- cov_complete_data %>%
  filter(confirmed >= 1) %>%
  arrange(country, date) %>% 
  mutate(widespread = ifelse(confirmed >= start_confirm_cases, T, F)) %>%
  group_by(country) %>%
  mutate(day_of_spread = cumsum(widespread)) %>%
  filter(day_of_spread != 0) %>%
  mutate(spread_index = confirmed/first(confirmed)) %>%
  filter(spread_index >= 1)

options(scipen = 10000)

plotly::ggplotly(
  ggplot(data = cov_significant_confirmed_cases, aes(x = day_of_spread, y = spread_index)) +
    geom_line(aes(colour = country)) +
    scale_x_continuous(breaks = seq(1, 700, 7), labels = seq(1, 700, 7)) +
    scale_y_log10() +
    xlab(paste("Дней с момента выявления первых", start_confirm_cases, "случаев")) +
    ylab("Скорость распространения")
)
