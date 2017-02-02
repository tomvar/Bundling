# The TC cost function:
# inputs:
# n.1 - number of units of good 1
# n.2 - number of units of good 2
# c.1 - cost parameter of good 1
# c.2 - cost parameter of good 2
# alfa - parameter of scale economics alfa = 0 --> CRS, alfa < 0 --> IRS, alfa < 0 --> DRS
# beta - parameter of sope economics beta = 0 --> neutral, beta > 0 complementary, beta < 0 substitution
# FC - fixed cost


Cost.Bundle  <- function(n.1,n.2,c.1,c.2,alfa,beta,FC){

  cost <- ifelse(n.1==n.2, ((1-beta)*(c.1*n.1+c.2*n.2))^(1- alfa),
          ifelse(n.1 > n.2, (c.1*(n.1-n.2) + (1-beta)*(c.1*n.2 + c.2*n.2))^(1- alfa),
                                   (c.2*(n.2-n.1) + (1-beta)*(c.1*n.1 + c.2*n.1))^(1- alfa))) + FC

  return(cost) }
