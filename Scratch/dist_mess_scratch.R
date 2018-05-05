# Script to test out some stuff relating to visualizing distributions

library(tidyverse)
library(grid)  # trying the brackets thing
library(pBrackets)


# Dist stuff ----
n_sim <- 10^5



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
  geom_vline(aes(xintercept = quantile(x, prob = .99)), color = "red", linetype = "dashed") + 
  coord_cartesian(xlim = c(0,9000000))

# Density function look ----
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


# Curly Brace Try ----
# Trying to put curly braces for annotation
# from:
# https://stackoverflow.com/questions/35633239/add-curly-braces-to-ggplot2-and-then-use-ggsave

bracketsGrob <- function(...){
  l <- list(...)
  e <- new.env()
  e$l <- l
  grid::grid.record(  {
    do.call(grid.brackets, l)
  }, e)
}

# grid.brackets(x1, y1, x2, y2, h = NULL, ticks = 0.5, curvature = 0.5, type = 1, col = 1, lwd = 1, lty = "solid")


bracket1 <- bracketsGrob(x1 = 0.33,
                         y1 = 0.05, 
                         x2 = 0, 
                         y2 = 0.05,
                         h = 0.05,
                         lwd = 2,
                         col = "red"
                         )
# add it to the plot
plot_mean_99var + 
  annotation_custom(bracket1)

plot_mean_99var + annotation_custom(grid.brackets(x1 = 0.33,
 y1 = 0.05, 
 x2 = 0, 
 y2 = 0.05,
 h = 0.05,
 lwd = 2,
 col = "red"
))

# That didn't seem to work.
# The answer had a link to a more basic answer, trying that:
# https://stackoverflow.com/questions/7001799/ggplot2-curly-braces-on-an-axis/33544572#33544572

plot_mean_99var

grid.locator(unit = "native") # captures location of mouse click on plot

bottom_y <- 37
grid.brackets(134, bottom_y, 206, bottom_y, lwd=2, col="red")

bottom_y <- 278
grid.brackets(206, bottom_y, 134, bottom_y, lwd=2, col="red")
 
# put lower x first for bracket opening down, higher first for braket opening up.

grid.brackets(206, bottom_y, 134, bottom_y + 20, lwd=2, col="red") 

# These units depend on size of output device. Resizing window makes them not line up correctly.


