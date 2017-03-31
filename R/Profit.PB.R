#' Profit, Consumer surplus, cost, demands for Pure Bundling strategy

#' @param r1.r2 N valuations of good 1 and 2  - reserversion prices r1, r2
#' @param price.pc Monopoly price of bundle [price.pc <- c(p.b)]
#' @param c.1  Parameter of production cost of good 1
#' @param c.2  Parameter of production cost of good 1
#' @param alfa Parameter of scale economics alfa = 0 --> CRS, alfa < 0 --> IRS, alfa < 0 --> DRS
#' @param beta Parameter of sope economics  beta = 0 --> neutral, beta > 0 complementary in proiduction, beta < 0 substitution in production
#' @param teta Parameter of complementary and substitution of goods beta = 0 --> neutral, beta > 0 complementary in consumption, beta < 0 substitution in consumption
#' @param FC Fixed Cost of production
#'
#' @return For a given: i. N valuations of r1 ,r2 ii. Monopoly prices p1, p2 iii. Technology
#'  profit  - profit for Pure Bundle strategy
#'  c.s     - Consumer surplus for Pure Componenets strategy
#'  t.c     - cost of production
#'  no.buy  - number of consumers which do not buy any good
#'  buy.b   - number of consumers which buy bundle of good 1 and good 2
#'
#' @export

Profit.PB <- function(r1.r2, prices.pb, c.1, c.2, alfa, beta, teta, FC)
{
  p.b <- prices.pb

  no.buy <- as.matrix(r1.r2[(1 + teta) * (r1.r2[, 1] + r1.r2[,2]) < p.b &
                              (r1.r2[,1] + ifelse(r1.r2[,2]< 0,r1.r2[,2],0)   < p.b) &
                              (r1.r2[, 2] + ifelse(r1.r2[,1]< 0,r1.r2[,1],0) < p.b),])
  buy.b <- as.matrix(r1.r2[((1 + teta) * (r1.r2[, 1] + r1.r2[,2]) >= p.b) |
                             (r1.r2[,1] + ifelse(r1.r2[,2]< 0,r1.r2[,2],0) >= p.b) |
                             (r1.r2[, 2] + ifelse(r1.r2[,1]< 0,r1.r2[,1],0) >= p.b),])
  n.1 <- dim(buy.b)[1]
  n.2 <- n.1

  t.c <- Cost.Bundle(n.1, n.2, c.1, c.2, alfa, beta, FC)
  profit <- p.b * n.1 - t.c
  c.s <- apply(buy.b, 2, sum)[1] + apply(buy.b, 2, sum)[2]

  output.PB <- list(
    profit = profit,
       c.s = c.s,
       t.c = t.c,
       p.b = p.b,
    no.buy = no.buy,
    buy.b = buy.b )

  return(output.PB)
}
