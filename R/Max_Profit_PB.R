Max.Profit.PB  <- function(r1.r2, pb.min.max, c.1, c.2, alfa, beta,teta, FC) {

  numerate <- max(pb.min.max)

  FC  <- FC/numerate
  c.1 <- c.1/numerate
  c.2 <- c.2/numerate
  r1.r2 <- data.frame(r1.r2)/numerate

  pb.min.max <- pb.min.max/numerate
  step <- 0.001
  prices.pb <- Prices.PB(pb.min.max, step)


  output.i <-foreach(i = prices.pb, .combine="rbind",.packages = "bundling", .multicombine=TRUE) %dopar% {
    p.b <- i  # price Pure Bundling
    output <- Profit.PB(r1.r2, p.b, c.1, c.2, alfa, beta, teta, FC)
    list(output$profit,output$c.s,output$t.c,output$p.b )

  }

  output     <- matrix(unlist(output.i), ncol = 4, byrow = FALSE)
  ind.max.profit    <- apply(output, 2, max)[1]
  max.profit <- matrix((output[output[,1] == ind.max.profit]), ncol = 4, byrow = FALSE)
  ind.max.c.s    <- apply(max.profit, 2, max)[2]
  max.profit <- matrix((max.profit[max.profit[,2] == ind.max.c.s]),ncol = 4, byrow = FALSE)

  ndx <- order(abs( 0 - output[,1]))[1:(round(nrow(output)/10, digits = 0))]
  zero.profit <- output[ndx,]
  ind.max.c.s <- apply(zero.profit, 2, max)[2]
  zero.profit <- matrix((zero.profit[zero.profit[,2] == ind.max.c.s]),ncol = 4, byrow = FALSE)

  remove("output")


  output.max.PB   <-
    list(

      max.profit         = max.profit[1,1]*numerate,
      max.profit.c.s     = max.profit[1,2]*numerate,
      max.profit.t.c     = max.profit[1,3]*numerate,
      max.profit.p.b     = max.profit[1,4]*numerate,

      zero.profit.aprox   = zero.profit[1,1]*numerate,
      zero.profit.c.s     = zero.profit[1,2]*numerate,
      zero.profit.t.c     = zero.profit[1,3]*numerate,
      zero.profit.p.b     = zero.profit[1,4]*numerate)

  return(output.max.PB)
}
