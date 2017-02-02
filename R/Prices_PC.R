# This function create wector of prices for Pure Components

Prices.PC  <- function(p1.min.max, p2.min.max, step) {

  p1.min     <- p1.min.max[1]     # min price of good 1
  p1.max     <- p1.min.max[2]     # max price of good 1

  p2.min     <- p1.min.max[1]     # min price of good 2
  p2.max     <- p1.min.max[2]     # max price of good 2

  p1 <-seq(p1.min,p1.max ,by=step)
  p2 <-seq(p2.min,p2.max ,by=step)
  fullp1 <-rep(p1, each  = length(p1))
  fullp2 <-rep(p2, times = length(p2))
  p1.p2  <-cbind(fullp1, fullp2)

  return(p1.p2)}
