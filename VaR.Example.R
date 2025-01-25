
#INITIAL PRICES
prices.vec <- c(80.0400, 438.9000, 2717.8000)
#MEAN RETURNS
mean.vec <- c(.01, .01, .01)
#VOLATILITIES
sigma.vec <- c(0.019258352, 0.013240624, 0.009363993)

#CORRELATION MATRIX
cor.mat = matrix(
  c(1, 0.22, .18, .22, 1, 0.42, 0.18, 0.42, 1), 
  nrow = 3,   
  ncol = 3,         
  byrow = TRUE          
)

rownames(cor.mat) = c("WTI", "COPPER", "GOLD")
colnames(cor.mat) = c("WTI", "COPPER", "GOLD")


#Simulation functions
#Single price simulation
GBM <- function(N, sigma, mu, S0, Wt = NULL) {
  if (is.null(Wt)) {
    Wt <- cumsum(rnorm(N, 0, 1))  #cumulative Brownian motion
  }  
  t <- (1:N)/365   
  drift <- (mu - 0.5*(sigma^2)) * t
  random <- sigma * Wt
  St = S0 * exp(drift + random)   #drift = corrected drift term, random = random Brownian motion scaled by volatility
  return(St)
}


#Multiple correlated prices simulation
CorrelatedGBM <- function(N, S0, mu, sigma, cor.mat) {
  
  mu <- as.matrix(mu)  #average daily asset price drift
  sigma <- as.matrix(sigma)  #vol of daily log returns
  GBMs <- matrix(nrow = N, ncol = nrow(mu))  #row = # of days in path, ncol = # of correlated assets in simulation
  Wt <- matrix(rnorm(N * nrow(mu), 0, 1), ncol = nrow(mu))  #matrix of individual assets' daily RANDOM volatilities; simulated from Normal(0,1)
  Wt <- apply(Wt, 2, cumsum)   #apply cumulative daily sum of volatilities; each day's price contains the cumulative volatility up to that point
  chol.mat <- chol(cor.mat) # upper triangular cholesky decomposition of correlation matrix; use upper triangular matrix because observations are in rows; When the observations are in columns, multiply from the left with the lower triangular matrix. When the observations are in rows, multiply from the right with the upper triangular matrix
  Wt <- Wt %*% chol.mat   #matrix multiplication of matrix of cumulative sum of random volatility with cholesky decomposition of correlation matrix; Geometrically, the Cholesky matrix transforms uncorrelated variables into variables whose variances and covariances are given by Σ
  for (i in 1:nrow(mu)) {
    GBMs[,i] <- GBM(N, sigma[i], mu[i] , S0[i], Wt[,i])
  }
  return(GBMs)
}

#correlated asset paths and graph
cor.prices.sim <- CorrelatedGBM(N=5, prices.vec, mean.vec, sigma.vec, cor.mat)  #run function, save price simulations
class(cor.prices.sim)
cor.prices.sim <- data.frame(cor.prices.sim)
tail(cor.prices.sim)
colnames(cor.prices.sim) <- c("WTI", "COPPER", "GOLD")
cor.prices.sim$day <- 1:nrow(cor.prices.sim)
library(reshape2)
cor.prices.sim.melt <- dplyr::select(cor.prices.sim, day, "WTI", "COPPER", "GOLD") %>% melt(id.vars = "day")
(cor.prices.sim.gr <- ggplot(data = na.omit(cor.prices.sim.melt), aes(x = day, y = value, color = variable)) + geom_line() + scale_y_continuous(labels = scales::dollar) + ylab("") + xlab("") 
  + labs(title = "Assets Simulation\nhistorical correlations") + theme(legend.title = element_blank(), legend.position = c(.7,.8), legend.direction = "horizontal", legend.text = element_text(size = 12)) 
  + theme(text = element_text(size = 12))) 

#Multiple correlated prices simulation - final price only
CorrelatedGBM.last <- function(N, S0, mu, sigma, cor.mat) {
  
  mu <- as.matrix(mu)  #average daily asset price drift
  sigma <- as.matrix(sigma)  #vol of daily log returns
  GBMs <- matrix(nrow = N, ncol = nrow(mu))  #row = # of days in path, ncol = # of correlated assets in simulation
  Wt <- matrix(rnorm(N * nrow(mu), 0, 1), ncol = nrow(mu))  #matrix of individual assets' daily volatilities
  Wt <- apply(Wt, 2, cumsum)   #apply cumulative daily sum of volatilities; each day's price contains the cumulative volatility up to that point
  chol.mat <- chol(cor.mat) #upper triangular cholesky decomposition of correlation matrix
  Wt <- Wt %*% chol.mat   #matrix multiplication of matrix of cumulative sum of random volatility with cholesky decomposition of correlation matrix; Geometrically, the Cholesky matrix transforms uncorrelated variables into variables whose variances and covariances are given by Σ
  for (i in 1:nrow(mu)) {
    GBMs[,i] <- GBM(N, sigma[i], mu[i] , S0[i], Wt[,i])
  }
  return(tail(GBMs,1))
}


#Multiple simulations of LAST PRICE function
#store each row of LAST PRICES in matrix
N.5 <- 5
port.fwd.last.price.mc.5 <- do.call(rbind, lapply(1:10000, function(x) CorrelatedGBM.last(N=N.5, prices.vec, mean.vec, sigma.vec, cor.mat)))
colnames(port.fwd.last.price.mc.5) <- c("WTI", "COPPER", "GOLD")
#convert matrix to data.frame
port.fwd.last.price.mc.df.5 <- data.frame(port.fwd.last.price.mc.5)
port.fwd.last.price.mc.mtm.5 <- data.frame(port.fwd.last.price.mc.5)
quantile(port.fwd.last.price.mc.5[,1], c(.05, .01)) #95% and 99% quantiles: WTI
quantile(port.fwd.last.price.mc.5[,2], c(.05, .01)) #95% and 99% quantiles: COPPER
quantile(port.fwd.last.price.mc.5[,3], c(.05, .01)) #95% and 99% quantiles: GOLD


strikes <- c(75, 410, 2675)
size <- c(1000, 100, 2)

port.fwd.last.price.mc.mtm.strikes.5 <- sweep(port.fwd.last.price.mc.mtm.5, 2, strikes, '-')  #negative because sweep subtracts strikes from prices
port.fwd.last.price.mc.mtm.strikes.total.5 <- sweep(port.fwd.last.price.mc.mtm.strikes.5, 2, size, '*')
port.fwd.last.price.mc.mtm.strikes.5$mtm.total <- rowMeans(port.fwd.last.price.mc.mtm.strikes.5)
quantile(port.fwd.last.price.mc.mtm.strikes.5[,1], c(.05, .01))
port.fwd.last.price.mc.mtm.strikes.total.5$mtm.total <- rowSums(port.fwd.last.price.mc.mtm.strikes.total.5)
tail(port.fwd.last.price.mc.mtm.strikes.5)
tail(port.fwd.last.price.mc.mtm.strikes.total.5)


#5 DAY VAR GRAPHs
#MtM historgram with VaR calculations
quantile(port.fwd.last.price.mc.mtm.strikes.total.5$mtm.total, c(.05, .01))


(var.mtm.total.hist.5 <- ggplot(port.fwd.last.price.mc.mtm.strikes.total.5, aes(mtm.total)) + geom_histogram(fill="blue", bins = 100) + scale_x_continuous(labels = scales::dollar, breaks = seq(from=-50000, to = 50000, by=5000)) 
  + xlab("MtM value") + ylab("") + labs(title = "Portfolio Value at Risk, 5 day") 
  + geom_vline(xintercept = quantile(port.fwd.last.price.mc.mtm.strikes.total.5$mtm.total, .05), color = "black") 
  + annotate(geom = "text", label = paste("95% VaR =", scales::dollar(quantile(port.fwd.last.price.mc.mtm.strikes.total.5$mtm.total, .05))), x=quantile(port.fwd.last.price.mc.mtm.strikes.total.5$mtm.total, .05), y=400, vjust=1, hjust=-.05) 
  + geom_vline(xintercept = quantile(port.fwd.last.price.mc.mtm.strikes.total.5$mtm.total, .01), color = "red") 
  + annotate(geom = "text", color = "red", label = paste("99% VaR =", scales::dollar(quantile(port.fwd.last.price.mc.mtm.strikes.total.5$mtm.total, .01))), x=quantile(port.fwd.last.price.mc.mtm.strikes.total.5$mtm.total, .01), y=300, vjust=1, hjust=1))

