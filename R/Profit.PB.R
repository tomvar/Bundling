#' Profit function for PB
#'
#' @param r1.r2 NX2 reservation prices of two goods []
#' @param prices.pb - matrix of prices
#' @param c.1 good 1 parameter of production cost
#' @param c.2 good 1 parameter of production cost
#' @param alfa parameter of scale economics alfa = 0 --> CRS, alfa < 0 --> IRS, alfa < 0 --> DRS
#' @param beta parameter of sope economics  beta = 0 --> neutral, beta > 0 complementary, beta < 0 substitution
#' @param teta parameter of complementary and substitution of goods beta = 0 --> neutral, beta > 0 complementary, beta < 0 substitution
#' @param FC fixed Cost of production
#'
#' @return max profit
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
