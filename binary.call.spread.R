
#CALL SPREAD DELTA UNDER VARIOUS UNDERLYING PRICE ASSUMPTIONS
call.fcn.1 <- function(S){
  
  #S = 5980
  K = 6100
  r = .04
  T = 5/365
  vol = .15
  
  d1 <- (log(S/K) + (r + vol^2/2)*T) / (vol*sqrt(T))
  d2 <- d1 - vol*sqrt(T)
  euro.call.prem = S*pnorm(d1) - K*exp(-r*T)*pnorm(d2)
  binary.prem <- pnorm(-d2)*exp(-r*T)
  euro.call.delta = pnorm(d1)
  return(euro.call.delta)
  #return(list(euro.call.prem, binary.prem, euro.call.delta))
}

call.fcn.2 <- function(S){
  
  #S = 5980
  K = 6110
  r = .04
  T = 5/365
  vol = .15
  
  d1 <- (log(S/K) + (r + vol^2/2)*T) / (vol*sqrt(T))
  d2 <- d1 - vol*sqrt(T)
  euro.call.prem = S*pnorm(d1) - K*exp(-r*T)*pnorm(d2)
  binary.prem <- pnorm(-d2)*exp(-r*T)
  euro.call.delta = pnorm(d1)
  return(euro.call.delta)
  #return(list(euro.call.prem, binary.prem, euro.call.delta))
}

#call.fcn(.0877)
priceVector <- seq(5950, 6150, by = 1) #range of underlying prices
call.delta.1 <- sapply(priceVector, call.fcn.1)
call.delta.2 <- sapply(priceVector, call.fcn.2)

call.delta.df.1 <- data.frame(priceVector, call.delta.1)
colnames(call.delta.df.1) <- c("price", "call.delta.1")
call.delta.df.2 <- data.frame(priceVector, call.delta.2)
colnames(call.delta.df.2) <- c("price", "call.delta.2")

call.spread.delta <- inner_join(call.delta.df.1, call.delta.df.2, by = "price") |> dplyr::mutate(delta.spread = call.delta.1 - call.delta.2)
head(call.spread.delta)


library(ggplot2)
ggplot(call.spread.delta, aes(vol, delta.spread)) + geom_line(color = "red") + scale_y_continuous(labels = comma) + ggtitle("6100/6110 Call Spread Delta under different underlying price assumptions")





#CALL SPREAD DELTA UNDER VARIOUS VOL ASSUMPTIONS
call.fcn.1 <- function(vol){
  
  S = 5980
  K = 6100
  r = .04
  T = 5/365
  
  d1 <- (log(S/K) + (r + vol^2/2)*T) / (vol*sqrt(T))
  d2 <- d1 - vol*sqrt(T)
  euro.call.prem = S*pnorm(d1) - K*exp(-r*T)*pnorm(d2)
  binary.prem <- pnorm(-d2)*exp(-r*T)
  euro.call.delta = pnorm(d1)
  return(euro.call.delta)
  #return(list(euro.call.prem, binary.prem, euro.call.delta))
}

call.fcn.2 <- function(vol){
  
  S = 5980
  K = 6110
  r = .04
  T = 5/365
  
  d1 <- (log(S/K) + (r + vol^2/2)*T) / (vol*sqrt(T))
  d2 <- d1 - vol*sqrt(T)
  euro.call.prem = S*pnorm(d1) - K*exp(-r*T)*pnorm(d2)
  binary.prem <- pnorm(-d2)*exp(-r*T)
  euro.call.delta = pnorm(d1)
  return(euro.call.delta)
  #return(list(euro.call.prem, binary.prem, euro.call.delta))
}


volVector <- seq(.10, .60, by = .01) #range of vols
call.delta.1 <- sapply(volVector, call.fcn.1)
call.delta.2 <- sapply(volVector, call.fcn.2)

call.delta.df.1 <- data.frame(volVector, call.delta.1)
colnames(call.delta.df.1) <- c("vol", "call.delta.1")
call.delta.df.2 <- data.frame(volVector, call.delta.2)
colnames(call.delta.df.2) <- c("vol", "call.delta.2")

call.spread.delta <- inner_join(call.delta.df.1, call.delta.df.2, by = "vol") |> dplyr::mutate(delta.spread = call.delta.1 - call.delta.2)
head(call.spread.delta)


library(ggplot2)
ggplot(call.spread.delta, aes(vol, delta.spread)) + geom_line(color = "red") + scale_y_continuous(labels = dollar) + ggtitle("6100/6110 Call Spread Delta under different volatility assumptions")

