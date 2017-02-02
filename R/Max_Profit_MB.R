Max.Profit.MB  <- function(r1.r2,p.1.min.max, p.2.min.max, p.b.min.max,c.1,c.2,alfa,beta,teta,FC) {

  numerate <- max(p.1.min.max,p.2.min.max)

  FC  <- FC/numerate
  c.1 <- c.1/numerate
  c.2 <- c.2/numerate
  r1.r2 <- data.frame(r1.r2)/numerate

  p.1.min.max <- p.1.min.max/numerate
  p.2.min.max <- p.2.min.max/numerate
  p.b.min.max <- p.b.min.max/numerate

  ########################

  step <-  0.05
  prices.mb <- Prices.MB(p.1.min.max, p.2.min.max, p.b.min.max, step)

  output.i <-foreach(i = prices.mb[,1], j = prices.mb[,2], k = prices.mb[,3], .combine="rbind",.packages = "bundling", .multicombine=TRUE) %dopar% {

    p.1  <- i
    p.2  <- j
    p.b  <- k
    p.mb <- cbind(p.1,p.2,p.b)
    output <- Profit.MB(r1.r2, p.mb, c.1, c.2, alfa, beta, teta, FC)

    list(output$profit,output$c.s,output$t.c,output$p.1,output$p.2,output$p.b)}

  output     <- matrix(unlist(output.i), ncol = 6, byrow = FALSE)
  ind.max.profit    <- apply(output, 2, max)[1]
  max.profit <- matrix((output[output[,1] == ind.max.profit]), ncol = 6, byrow = FALSE)
  ind.max.c.s    <- apply(max.profit, 2, max)[2]
  max.profit <- matrix((max.profit[max.profit[,2] == ind.max.c.s]),ncol = 6, byrow = FALSE)

  ########################

  ndx <- order(abs( 0 - output[,1]))[1:(round(nrow(output)/10, digits = 0))]
  zero.profit <- output[ndx,]
  ind.max.c.s <- apply(zero.profit, 2, max)[2]
  zero.profit <- matrix((zero.profit[zero.profit[,2] == ind.max.c.s]),ncol = 6, byrow = FALSE)

  ########################

  step <-  0.01
  p.1.min.max <- c(max.profit[1,4]-0.1,max.profit[1,4]+0.1)
  p.2.min.max <- c(max.profit[1,5]-0.1,max.profit[1,5]+0.1)
  p.b.min.max <- c(max.profit[1,6]-0.1,max.profit[1,6]+0.1)
  prices.mb <- Prices.MB(p.1.min.max, p.2.min.max, p.b.min.max, step)

  output.i <-foreach(i = prices.mb[,1], j = prices.mb[,2], k = prices.mb[,3], .combine="rbind",.packages = "bundling", .multicombine=TRUE) %dopar% {

    p.1  <- i
    p.2  <- j
    p.b  <- k
    p.mb <- cbind(p.1,p.2,p.b)
    output <- Profit.MB(r1.r2, p.mb, c.1, c.2, alfa, beta, teta, FC)

    list(output$profit,output$c.s,output$t.c,output$p.1,output$p.2,output$p.b)}

  output     <- matrix(unlist(output.i), ncol = 6, byrow = FALSE)
  ind.max.profit    <- apply(output, 2, max)[1]
  max.profit <- matrix((output[output[,1] == ind.max.profit]), ncol = 6, byrow = FALSE)
  ind.max.c.s    <- apply(max.profit, 2, max)[2]
  max.profit <- matrix((max.profit[max.profit[,2] == ind.max.c.s]),ncol = 6, byrow = FALSE)


  ########################

  step <-  0.001
  p.1.min.max <- c(max.profit[1,4]-0.01,max.profit[1,4]+0.01)
  p.2.min.max <- c(max.profit[1,5]-0.01,max.profit[1,5]+0.01)
  p.b.min.max <- c(max.profit[1,6]-0.01,max.profit[1,6]+0.01)
  prices.mb <- Prices.MB(p.1.min.max, p.2.min.max, p.b.min.max, step)

  output.i <-foreach(i = prices.mb[,1], j = prices.mb[,2], k = prices.mb[,3], .combine="rbind",.packages = "bundling", .multicombine=TRUE) %dopar% {

    p.1  <- i
    p.2  <- j
    p.b  <- k
    p.mb <- cbind(p.1,p.2,p.b)
    output <- Profit.MB(r1.r2, p.mb, c.1, c.2, alfa, beta, teta, FC)

    list(output$profit,output$c.s,output$t.c,output$p.1,output$p.2,output$p.b)}

  output     <- matrix(unlist(output.i), ncol = 6, byrow = FALSE)
  ind.max.profit    <- apply(output, 2, max)[1]
  max.profit <- matrix((output[output[,1] == ind.max.profit]), ncol = 6, byrow = FALSE)
  ind.max.c.s    <- apply(max.profit, 2, max)[2]
  max.profit <- matrix((max.profit[max.profit[,2] == ind.max.c.s]),ncol = 6, byrow = FALSE)

  remove("output")

  output.max.MB   <-
    list( max.profit         = max.profit[1,1]*numerate,
          max.profit.c.s     = max.profit[1,2]*numerate,
          max.profit.t.c     = max.profit[1,3]*numerate,
          max.profit.p.1     = max.profit[1,4]*numerate,
          max.profit.p.2     = max.profit[1,5]*numerate,
          max.profit.p.b     = max.profit[1,6]*numerate,

          zero.profit.aprox   = zero.profit[1,1]*numerate,
          zero.profit.c.s     = zero.profit[1,2]*numerate,
          zero.profit.t.c     = zero.profit[1,3]*numerate,
          zero.profit.p.1     = zero.profit[1,4]*numerate,
          zero.profit.p.2     = zero.profit[1,5]*numerate,
          zero.profit.p.b     = zero.profit[1,6]*numerate)

  return(output.max.MB)}
