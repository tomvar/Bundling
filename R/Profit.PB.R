# Profit function for PB
# Input
# r1.r2 - Matrix of reservation prices
# prices.mb - matrix of prices
# c.1 - cost parameter of good 1
# c.2 - cost parameter of good 2
# alfa - parameter of scale economics alfa = 1 --> CRS, alfa < 1 --> DRS
# beta - parameter of sope economics beta = 0 --> neutral, beta < 0 complementary, beta > 0 substitution
# FC - fixed cost

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
