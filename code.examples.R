

#PREDICT YOY SP500 WITH YOY MACRO INDICATORS
#LINEAR REGRESSION: SPY ~ EACH INDIVIDUAL PREDICTOR
#SORT BY P-VALUE
df.loop <- indic.all.df |> dplyr::select("spy.yoy", "lei.yoy", "coi.yoy", "bberg.fcon.yoy.diff", "avg.manu.hrs.yoy", "weekly.unem.yoy", "manu.new.orders.yoy", "ism.manu.pmi.yoy", "ism.svcs.pmi.yoy", "ism.new.orders.pmi.yoy", "manu.new.orders.nondef.yoy", "new.perm.housing.yoy", "leading.credit.index.yoy.diff", "rate.spread.yoy", "avg.cons.exp.yoy.diff", "diffusion.yoy", "cons.exp.yoy", "cons.conf.yoy", "ted.sprd.yoy.diff", "bsby.ois.yoy.diff", 
                                         "commpaper.3mtbill.yoy.diff", "baacorp.10yrtreas.yoy.diff", "hy.10yrtreas.yoy.diff", "usmuni.10yrtreas.yoy.diff", "usd.3m10y.swaption.vol.yoy.diff", "sp500.vix.yoy.diff", "fed.rate.yoy.diff", "us.cpi.yoy.diff",
                                         "us6m10yr.yoy.diff", "us2yr10yr.yoy.diff", "us6m.yoy.diff", "us2y.yoy.diff", "us10y.yoy.diff", "us30y.yoy.diff", "infl.be.2yr.yoy.diff", "infl.be.5yr.yoy.diff",
                                         "infl.be.7yr.yoy.diff", "infl.be.10yr.yoy.diff", "infl.be.20yr.yoy.diff")

lm.test <- map_dfr(
  set_names(names(df.loop)[-1]),
  ~ glance(lm(
    as.formula(paste("spy.yoy ~", .x)),
    data = df.loop
  )),
  .id = "predictor"
)

lm.test
arrange(lm.test, p.value)
lm.test |> dplyr::filter(p.value >= .05)
lm.test |> dplyr::filter(p.value < .05) |> arrange(p.value)



#ROLLING R^2
#LEI YOY R2
(nr <- nrow(indic.all.df))
r.2.fcn <- function(x) summary(lm(spy.ret.lead ~ lei.yoy, indic.all.df, subset = x))$adj.r.squared
mod4 <- rollapplyr(1:nr, 36, r.2.fcn, fill = NA)
mod4.df <- data.frame(mod4)
ggplot(indic.all.df, aes(date, mod4)) + geom_line() + geom_hline(yintercept = .05, color = "red")



#ROLLING BETA
betafun <- function(x) cov(Re(x), Im(x)) / var(Im(x))
et.es1.roll.beta <- et.port.es1.merge.ret %>% mutate(et.beta = rollapplyr(SEI + SP500 * 1i, 250, betafun, fill = NA))



#QUARTERLY PUT OPTION TAIL HEDGE ANALYSIS
#INTRA-QUARTER DOWNSIDE SEVERITY
#ES
library(Rblpapi)
library(tidyverse)
library(lubridate)
library(xts)
blpConnect()

es.im.bb <- bdh("ES1 INDEX", c("PX_Last", "FUT_CUR_GEN_TICKER", "CURRENT_CONTRACT_MONTH_YR"), start.date = as.Date("2000-01-01"))
head(es.im.bb)

es.im.df <- es.im.bb
colnames(es.im.df) <- c("date", "es", "symbol", "contract.month.yr")
head(es.im.df)

es.im <- xts(es.im.df[,-1], es.im.df$date)
colnames(es.im) <- c("es", "symbol", "contract.month.yr")
head(es.im)
(es.im.first <- head(es.im,1))
es.im.df$year <- year(es.im.df$date)
es.im.df$month <- month(es.im.df$date)
es.im.df$day <- mday(es.im.df$date)
es.im.df$qtr <- lubridate::quarter(es.im.df$date)
es.im.df$year.mon <- zoo::as.yearmon(paste(es.im.df$year, es.im.df$month), "%Y %m")
es.im.df$year.qtr <- zoo::as.yearmon(paste(es.im.df$year, es.im.df$qtr), "%Y %m")
es.im.df$year.qtr <- paste(es.im.df$year, es.im.df$qtr, sep = "-")
es.im.df$lag.5 <- lag(es.im.df$es, 5)
es.im.df$lag.5.date <- lag(es.im.df$date, 5)

head(es.im.df,10)
tail(es.im.df,10)

#split into list of months
es.im.split <- split(es.im.df, es.im.df$year.qtr) 
class(es.im.split)
head(es.im.split)

es.im.split <- lapply(es.im.split, function(x) cbind(x, day1.delta = x$es - head(x$lag.5,1))) #intra-month daily change
es.im.split <- lapply(es.im.split, function(x) cbind(x, day1.delta.perc = (x$es / head(x$lag.5, 1)) - 1)) #intra-month daily change
es.im.split <- lapply(es.im.split, function(x) cbind(x, itm = ifelse(min(x$day1.delta.perc) <= -.10, 1, 0))) #intra-month daily change
es.im.split <- lapply(es.im.split, function(x) cbind(x, k.10 = head(x$lag.5, 1)*.90)) #intra-month daily change
es.im.split <- lapply(es.im.split, function(x) cbind(x, k.15 = head(x$lag.5, 1)*.85)) #intra-month daily change
es.im.split <- lapply(es.im.split, function(x) cbind(x, trade.date = head(x$lag.5.date, 1))) #intra-month daily change

#KEEP ONLY LISTS THAT WERE ITM
#rbind, then filter to remove otm months, then split again
es.im.rbind.itm <- do.call("rbind", es.im.split)
es.im.rbind.itm <- es.im.rbind.itm %>% dplyr::filter(itm == 1)
es.im.rbind.itm <- es.im.rbind.itm %>% arrange(es.im.rbind.itm$date)
head(es.im.rbind.itm, 15)

es.im.split.itm <- split(es.im.rbind.itm, es.im.rbind.itm$year.qtr)
head(es.im.split.itm, 15)
length(es.im.split.itm)
length(es.im.split)
length(es.im.split.itm)/length(es.im.split)

es.im.rbind <- do.call("rbind", es.im.split)   #combine all list members into one dataframe
head(es.im.rbind,25)
es.im.rbind <- es.im.rbind[!(is.na(es.im.rbind$symbol) | es.im.rbind$symbol == ""), ]   #REMOVE ROWS WITH BLANK FUTURES SYMBOL
es.im.rbind <- es.im.rbind %>% arrange(es.im.rbind$date)   #sort by date

(es.im.chart.qtr <- es.im.rbind %>% dplyr::filter(date >= "2024-05-01") %>% ggplot(aes(x=date, y=day1.delta.perc)) + geom_line() + geom_hline(yintercept = -.10, color = "red") + labs(title = "ES Intra-quarter volatility\nfirst day of front quarter price - each subsequent day price") + scale_y_continuous(labels = scales::percent, breaks = seq(-1,1, .05)))
(es.im.chart.qtr <- es.im.rbind %>% ggplot(aes(x=date, y=day1.delta.perc)) + geom_line() + geom_hline(yintercept = -.15, color = "red") + labs(title = "ES Intra-quarter volatility\nfirst day of quarter price - each subsequent day price throughout each quarter") + scale_y_continuous(labels = scales::percent, breaks = seq(-1,1, .05)))




#PORTFOLIO OF HEDGES + PHYSICAL PRODUCTION
#EVALUATE HEDGE RE-STRUCTURE UNDER VARIOUS VOL ASSUMPTIONS
expectedValueFnc <- function(vol) {
  
  #Jan21 40/48/53.30 collar
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

  #Jan21 sold call 53.30
  call.1 <- pmax(0, endPrice - call.strike.1)
  call.1.end <- mean(call.1)*CollarVolume*-1

  #jan21 bought put 48
  put.1 <- pmax(0, put.strike.1 - endPrice)
  mean(put.1)
  put.1.end <- mean(put.1)*CollarVolume
  
  #jan21 sold put 40
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
ggplot(voldf, aes(vol, breakevens)) + geom_hline(yintercept = 0) + geom_line(color = "red") + scale_y_continuous(labels = dollar) + ggtitle("$53.30 call buyback under different volatilities")


