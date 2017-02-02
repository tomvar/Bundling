# price of bundle
Prices.PB  <- function(pb.min.max, step) {

  pb.min     <- pb.min.max[1]     # min price of bundle
  pb.max     <- pb.min.max[2]     # max price of bundle

  out1 <- seq(pb.min,pb.max,by = step)

  return(p.b = out1)}
