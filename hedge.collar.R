#PORTFOLIO OF HEDGES + PHYSICAL PRODUCTION
#EVALUATE HEDGE RE-STRUCTURE UNDER VARIOUS VOL ASSUMPTIONS
expectedValueFnc <- function(vol) {
  
  wti <- 36.26
  tau <- 231/365
  rf <- .02
  set.seed(123)
  num.sim <- 100000
  r <- (rf - 0.5*vol^2)*tau
  sd <- vol*sqrt(tau)
  
  CollarVolume <- 1000*31
  call.1.Price = 1.10
  call.strike.1 <- 53.3
  put.strike.1 <- 48
  put.strike.2 <- 40
  swap.1.strike <- 40.92
  swap.2.strike <- 40.85
  swap.3.strike <- 40.48
  put.strike.3 <- 40.92
  put.strike.4 <- 40.85
  
  
  endPrice <- wti*exp(r + sd*rnorm(num.sim,0,1))
  mean(endPrice)
  
  #SOLD CALL
  call.1 <- pmax(0, endPrice - call.strike.1)
  call.1.end <- mean(call.1)*CollarVolume*-1
  
  #BOUGHT PUT
  put.1 <- pmax(0, put.strike.1 - endPrice)
  mean(put.1)
  put.1.end <- mean(put.1)*CollarVolume
  
  #SOLD PUT
  put.2 <- pmax(0, put.strike.2 - endPrice)
  mean(put.2)
  put.2.end <- mean(put.2)*CollarVolume*-1
  
  #collar total terminal value expectation
  collarvalue <- call.1.end + put.1.end + put.2.end   #total collar value
  
  #sold swaps
  swap.1.volume <- 1000*31*-1
  swap.1 <- endPrice - swap.1.strike
  swap.1 <- mean(swap.1)
  swap.1.end <- mean(swap.1*swap.1.volume)
  
  swap.2.volume <- 2000*31*-1
  swap.2 <- endPrice - swap.2.strike
  swap.2 <- mean(swap.2)
  swap.2.end <- mean(swap.2*swap.2.volume)
  
  swap.3.volume <- 2000*31*-1
  swap.3 <- endPrice - swap.3.strike
  swap.3 <- mean(swap.3)
  swap.3.end <- mean(swap.3*swap.3.volume)
  
  swaptotal <- swap.1.end + swap.2.end + swap.3.end #total swap value
  
  #deferred puts
  put.premium.3 <- 5.50
  put.3.volume <- 1000*31
  put.3 <- pmax(0, put.strike.3 - endPrice) - put.premium.3
  #mean(put.3)
  put.3.end <- mean(put.3)*put.3.volume
  
  put.premium.4 <- 5.50
  put.4.volume <- 2000*31
  put.4 <- pmax(0, put.strike.4 - endPrice) - put.premium.4
  #mean(put.4)
  put.4.end <- mean(put.4)*put.4.volume
  
  def.puts.total <- put.3.end + put.4.end
  
  
  #production
  prod <- 18000*31
  prodend <- mean(prod*endPrice)
  
  #total prod + hedges expected value
  prod.plus.hedges <- call.1.end + put.1.end + put.2.end + swap.1.end + swap.2.end + swap.3.end + put.3.end + put.4.end + prodend
  library(scales)
  dollar(prod.plus.hedges)
  
  #without call; call buyback scenario
  prod.no.call.1 <- put.1.end + put.2.end + swap.1.end + swap.2.end + swap.3.end + put.3.end + put.4.end + prodend
  
  #difference per barrel
  prod.no.call.1 - prod.plus.hedges
  dollar(prod.no.call.1 - prod.plus.hedges)
  
  prod.no.call.1 - prod.plus.hedges - call.1.Price*CollarVolume
  
  return(list(dollar(prod.plus.hedges), dollar(prod.no.call.1), dollar(prod.no.call.1 - prod.plus.hedges), dollar(call.1.Price*CollarVolume), (prod.no.call.1 - prod.plus.hedges) - call.1.Price*CollarVolume))
  
}

expectedValueFnc(vol = .37) #vol constant

volVector <- seq(.30, .50, by = .01) #range of vols

expectedValueVol <- sapply(volVector, expectedValueFnc)

breakevens <- unlist(expectedValueVol[5,])

voldf <- data.frame(volVector, breakevens)
colnames(voldf) <- c("vol", "breakevens")
voldf 

library(ggplot2)
ggplot(voldf, aes(vol, breakevens)) + geom_hline(yintercept = 0) + geom_line(color = "red") + scale_y_continuous(labels = dollar) + ggtitle("$53.30 call buyback under different volatility assumptions")


