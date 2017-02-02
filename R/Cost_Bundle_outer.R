# The outer of TC
# Inputs:
# n.1 - number of units of good 1
# n.2 - number of units of good 2
# c.1 - cost parameter of good 1
# c.2 - cost parameter of good 2
# alfa - parameter of scale economics alfa = 1 --> CRS, alfa < 1 --> DRS
# beta - parameter of sope economics beta = 0 --> neutral, beta < 0 complementary, beta > 0 substitution
# FC - fixed cost


Cost.Bundle.Outer <- function(N,c.1,c.2,alfa, beta, FC){
  cost.matrix <- matrix(0,nrow = N,ncol = N)
  for(i in 1:N){
    for(j in 1:N){
      cost.matrix[i,j] <- ifelse(i == j, ((1-beta)*(c.1*i+c.2*j))^(1- alfa) ,
                          ifelse(i >  j, (c.1*(i-j) + (1-beta)*(c.1*j+c.2*j))^(1- alfa),
                                               (c.2*(j-i)+(1-beta)*(c.1*i+c.2*i))^(1- alfa) ))}}

cost.matrix <- cbind(c(0,(c.1*(1:N))^(1- alfa)),rbind((c.2*(1:N))^(1- alfa),cost.matrix)) + FC
return(as.matrix(cost.matrix))
}
