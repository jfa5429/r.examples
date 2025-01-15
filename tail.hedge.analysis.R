#QUARTERLY PUT OPTION TAIL HEDGE ANALYSIS
#INTRA-QUARTER DOWNSIDE SEVERITY
#ES
library(Rblpapi)
library(tidyverse)
library(lubridate)
library(xts)
blpConnect()

#DATA FROM BLOOMBERG
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

#split into list of quarters
es.im.split <- split(es.im.df, es.im.df$year.qtr) 
class(es.im.split)
head(es.im.split)

es.im.split <- lapply(es.im.split, function(x) cbind(x, day1.delta = x$es - head(x$lag.5,1))) #intra-quarter daily dollar change vs first day
es.im.split <- lapply(es.im.split, function(x) cbind(x, day1.delta.perc = (x$es / head(x$lag.5, 1)) - 1)) #intra-quarter daily percent change vs first day
es.im.split <- lapply(es.im.split, function(x) cbind(x, itm = ifelse(min(x$day1.delta.perc) <= -.10, 1, 0))) #intra-quarter daily percent change less than neg 10%
es.im.split <- lapply(es.im.split, function(x) cbind(x, k.10 = head(x$lag.5, 1)*.90)) #beginning of quarter 10% OTM strike
es.im.split <- lapply(es.im.split, function(x) cbind(x, k.15 = head(x$lag.5, 1)*.85)) #beginning of quarter 15% OTM strike
es.im.split <- lapply(es.im.split, function(x) cbind(x, trade.date = head(x$lag.5.date, 1))) #hedge trade date

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

(es.im.chart.qtr <- es.im.rbind %>% ggplot(aes(x=date, y=day1.delta.perc)) + geom_line() + geom_hline(yintercept = -.15, color = "red") + labs(title = "ES Intra-quarter volatility\nfirst day of quarter price - each subsequent day price throughout each quarter") + scale_y_continuous(labels = scales::percent, breaks = seq(-1,1, .05)))

