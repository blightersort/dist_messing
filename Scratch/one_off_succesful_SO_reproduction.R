# Trying to exactly reproduce this answer:
# https://stackoverflow.com/questions/35633239/add-curly-braces-to-ggplot2-and-then-use-ggsave
# 

library(ggplot2)
library(grid)
library(pBrackets) 
x <- c(runif(10),runif(10)+2)
y <- c(runif(10),runif(10)+2)
the_plot <- qplot(x=x,y=y) +
  scale_x_continuous("",breaks=c(.5,2.5),labels=c("Low types","High types") ) +
  theme(axis.ticks = element_blank(),
        axis.ticks.length = unit(.85, "cm"))
the_plot
grid.locator(unit="native") 
bottom_y <- 284 
grid.brackets(220, bottom_y,   80, bottom_y, lwd=2, col="red")
grid.brackets(600, bottom_y,  440, bottom_y, lwd=2, col="red")



bracketsGrob <- function(...){
  l <- list(...)
  e <- new.env()
  e$l <- l
  grid:::recordGrob(  {
    do.call(grid.brackets, l)
  }, e)
}

# note that units here are "npc", the only unit (besides physical units) that makes sense
# when annotating the plot panel in ggplot2 (since we have no access to 
# native units)

b1 <- bracketsGrob(0.33, 0.05, 0, 0.05, h=0.05, lwd=2, col="red")
b2 <- bracketsGrob(1, 0.05, 0.66, 0.05, h=0.05,  lwd=2, col="red")

p <- the_plot + 
  annotation_custom(b1)+ 
  annotation_custom(b2) +
  scale_y_continuous(expand=c(0.11,0))
p



