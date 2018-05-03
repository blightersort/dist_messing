# Script to test out some stuff relating to visualizing distributions

library(tidyverse)

# Dist stuff ----
n_sim <- 10^6



# log normal

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




# Plotting stuff ----

plot_mean_99var <- ggplot(data = dist_sim, aes(x = x, y = ..density..)) +
  geom_histogram(bins = 1000) + 
  geom_density() +
  geom_vline(aes(xintercept = mean(x)), color = "red", linetype = "dashed") +
  geom_vline(aes(xintercept = quantile(x, prob = .99)), color = "red", linetype = "dashed")


# density apparently returns the density for plotting
# taking the idea of pulling the dnsity so you can do ribbon plots and other
# tricks with it from the WVPlots package

dens_sim <- dist_sim$x %>% 
  density(x = ., adjust = 0.5) 

dens_sim <- as_tibble(list(x = dens_sim$x, dens = dens_sim$y))

# demonstrating that they are the same
ggplot(data = dens_sim, aes( x = x, y = dens)) + geom_line(color = "red") 

ggplot() + geom_density(data = dist_sim, aes(x = x, y = ..density..), color= "blue")
  

# ecdf returns a FUNCTION that computes the empirical cdf
dens_fun <-  dist_sim$x %>% ecdf()

# for tail fills
# 1 for left tail, -1 for right
# sign = ifelse(tail=="left", 1, -1)
sign = -1

# threshold is the percentile we want
threshold = dist_sim$x %>% quantile(x = ., probs = (.99))
dens_sim$tail = ifelse(sign*dens_sim$x < sign*threshold, dens_sim$dens, 0)

# area = switch(tail,
#               left = densityfun(threshold),
#               right= 1- densityfun(threshold)
# )

area = 1 - dens_fun(threshold)











