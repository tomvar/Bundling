#' This function create wector of prices for Pure Components
#'
#' @param pb.min.max Vector
#'
#' @return wector of prices for Pure Bundling
#'
#' @export

Prices.PB  <- function(pb.min.max, step) {

  pb.min     <- pb.min.max[1]     # min price of bundle
  pb.max     <- pb.min.max[2]     # max price of bundle

  out1 <- seq(pb.min,pb.max,by = step)

  return(p.b = out1)}
