countries_to_plot <- c('Russia', 'Belgium', 'Netherlands', 'Italy', 'Israel', 'Germany')

ggplot(data = cov_complete_data[cov_complete_data$country %in% countries_to_plot, ], aes(x = date)) +
  geom_line(aes(y = confirmed_diff, group = country), colour = 'blue') +
  geom_line(aes(y = deaths_diff, group = country), colour = 'black') +
  scale_y_continuous(sec.axis = sec_axis(~ . / max(cov_complete_data$confirmed_diff, na.rm = T)) ) +
  facet_wrap(~ country, scales = 'free')