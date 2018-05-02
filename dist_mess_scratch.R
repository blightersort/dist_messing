# Script to test out some stuff relating to visualizing distributions

library(tidyverse)


# log normal

n_sim <- 10^6


mean_log <- 12
sd_log <- 1


dist_sim <- rlnorm(n = n_sim, meanlog = mean_log, sdlog = sd_log) %>%
  list(x = .) %>% as_tibble()

plot_mean_99var <- ggplot(data = dist_sim, aes(x = x, y = ..density..)) +
  geom_histogram(bins = 1000) + 
  geom_density() +
  geom_vline(aes(xintercept = mean(x)), color = "red", linetype = "dashed") +
  geom_vline(aes(xintercept = quantile(x, prob = .99)), color = "red", linetype = "dashed")





