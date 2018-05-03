# Script to test out some stuff relating to visualizing distributions

library(tidyverse)


# log normal

n_sim <- 10^6

mean_targ <- 1e6
cv_targ <- .5





mean_log <- log(mean_targ) - log(cv_targ^2 + 1) / 2
sd_log <- (cv_targ^2 + 1) %>% log() %>% sqrt()


dist_sim <- rlnorm(n = n_sim, meanlog = mean_log, sdlog = sd_log) %>%
  list(x = .) %>% as_tibble()

# mean test
mean(dist_sim$x)

# cv test
sd(dist_sim$x)/mean(dist_sim$x)


plot_mean_99var <- ggplot(data = dist_sim, aes(x = x, y = ..density..)) +
  geom_histogram(bins = 1000) + 
  geom_density() +
  geom_vline(aes(xintercept = mean(x)), color = "red", linetype = "dashed") +
  geom_vline(aes(xintercept = quantile(x, prob = .99)), color = "red", linetype = "dashed")





