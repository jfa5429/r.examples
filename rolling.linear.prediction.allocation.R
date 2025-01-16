#DATA IN "indic.all.df.csv" file

#SUBSET TARGET AND PREDICTOR DATA
df.loop.base <- indic.all.df |> dplyr::filter(date < "2025-01-01") |> dplyr::select(date, spy.mom.lead, lei.yoy, lei.mom.sma3, lei.mom.sma6) |> na.omit()
df.loop <- df.loop.base |> dplyr::select(-date)
df.loop.dates <- df.loop.base |> dplyr::select(date)



#ROLLING LINEAR REGRESSION FUNCTION: PREDICT NEXT MONTH SPY RETURN
stats <- function(x) {
  fm <- lm(spy.mom.lead ~ ., as.data.frame(x))
  lm.pred <- tail(predict(fm, newdata = as.data.frame(x)), 1)
  c(coef(fm), unlist(glance(fm)))
}


n.months <- 24   #NUMBER OF MONTHS FOR EACH STEP IN ROLLING REGRESSION
spy.pred.lead.roll.lm2 <- rollapplyr(df.loop, width = n.months, FUN = stats, by.column = FALSE)  #APPLY REGRESSION FUNCTION IN ROLLING 24 MONTH WINDOWS
tail(spy.pred.lead.roll.lm2)



n <- nrow(df.loop)
df.loop.fit <- df.loop[n.months:n,]
spy.pred.lead.roll.lm.df <- data.frame(spy.pred.lead.roll.lm)
spy.pred.lead.roll.lm.df$actual <- df.loop.fit$spy.mom
spy.pred.lead.roll.lm.df$actual.lead <- lead(df.loop.fit$spy.mom, 1)
#TRAIN: SPY LEAD ~ 
#PREDICT ON NEXT MONTH PREDICTORS
#add predictor leads to make predictions
spy.pred.lead.roll.lm.df$lei.yoy.actual.lead <- lead(df.loop.fit$lei.yoy, 1)
spy.pred.lead.roll.lm.df$lei.mom.sma3.actual.lead <- lead(df.loop.fit$lei.mom.sma3, 1)
spy.pred.lead.roll.lm.df$lei.mom.sma6.actual.lead <- lead(df.loop.fit$lei.mom.sma6, 1)

#ADD DATES
spy.pred.lead.roll.lm.df <- data.frame(tail(df.loop.dates, nrow(spy.pred.lead.roll.lm.df)), spy.pred.lead.roll.lm.df)
head(spy.pred.lead.roll.lm.df)
tail(spy.pred.lead.roll.lm.df)

#ADJUST PREDICTOR POSITION FOR DATE COLUMN
spy.pred.lead.roll.lm.df$pred <- spy.pred.lead.roll.lm.df$X.Intercept. + spy.pred.lead.roll.lm.df[,3]*spy.pred.lead.roll.lm.df$lei.yoy.actual.lead + spy.pred.lead.roll.lm.df[,4]*spy.pred.lead.roll.lm.df$lei.mom.sma3.actual.lead + spy.pred.lead.roll.lm.df[,5]*spy.pred.lead.roll.lm.df$lei.mom.sma6.actual.lead

#BASIC 4 ASSET SWITCHING MODEL
#SWITCH TO AGG WHEN PREDICTED RETURN IS LESS THAN THRESHOLD
agg.gld.xlp.mom.df <- indic.all.df |> dplyr::select(date, agg.mom, gld.mom, xlp.cons.def.mom) |> dplyr::mutate(agg.lead.mom = lead(agg.mom, 1), gld.lead.mom = lead(gld.mom, 1), xlp.lead.mom = lead(xlp.cons.def.mom, 1))
tail(agg.gld.xlp.mom.df)
tail(spy.pred.lead.roll.lm.df)
spy.agg.gld.xlp.pred.alloc <- inner_join(spy.pred.lead.roll.lm.df, agg.gld.xlp.mom.df, by = "date") |> na.omit()
head(spy.agg.gld.xlp.pred.alloc)
tail(spy.agg.gld.xlp.pred.alloc)

spy.agg.gld.xlp.pred.alloc <- spy.agg.gld.xlp.pred.alloc |> dplyr::mutate(pred.signal.allocation = case_when(
  pred <= 0 ~ 0.25*actual.lead + 0.25*agg.lead.mom + .25*gld.lead.mom + .25*xlp.lead.mom, 
  pred > 0 ~ 1*actual.lead
))


#CUMULATIVE STRATEGY RETURNS
spy.agg.gld.xlp.pred.alloc$spy.actual.lead.cumprod <- cumprod(1 + spy.agg.gld.xlp.pred.alloc$actual.lead) - 1
spy.agg.gld.xlp.pred.alloc$spy.agg.gld.xlp.lead.cumprod <- cumprod(1 + spy.agg.gld.xlp.pred.alloc$pred.signal.allocation) - 1


head(spy.agg.gld.xlp.pred.alloc)
tail(spy.agg.gld.xlp.pred.alloc)

#RETURN COMPARISON GRAPH
spy.agg.gld.xlp.pred.cumret.state.long.yoy <- spy.agg.gld.xlp.pred.alloc |> dplyr::select(date, spy.actual.lead.cumprod, spy.agg.gld.xlp.lead.cumprod) |> gather(key = "variable", value = "value", -date)
spy.agg.gld.xlp.pred.cumret.state.long.yoy |> ggplot(aes(x = date, y = value, color = variable)) + geom_line() + labs(title = "SPY PREDICTION MODEL", subtitle = "allocation: next month SPY return prediction < 0 -> 25% SPY 25% AGG 25% GLD 25% XLP, else 100% SPY")



#GRAPH PREDICTED VS ACTUAL RETURNS
spy.pred.lead.roll.lm.df.graph.lei.24m <- spy.pred.lead.roll.lm.df |> dplyr::mutate(color = factor(case_when(
  pred <= 0 ~ "red",
  pred > 0 ~ "green"
  #pred > -.02 & pred < .02 ~ "black"
)))

(spy.lead.roll.pred.lead.actual.lead.lei.24m.gr <- ggplot(spy.pred.lead.roll.lm.df.graph.lei.24m) +
    geom_point(aes(x=pred,y=actual.lead,color=color)) + 
    scale_color_manual(values=c("green", "red")) + stat_smooth(mapping = aes(x=pred, y = actual.lead), method = lm) +
    geom_vline(xintercept = 0.0, color = "black") + geom_hline(yintercept = 0, color = "black") + 
    labs(title = "SPY RETURN LEAD ~ LEI", subtitle = "Rolling 24 month regression on next month SPY return\n2000-today") + 
    scale_x_continuous(labels = scales::percent, breaks = seq(from=-.3, to=.3, by=.02)) +
    scale_y_continuous(labels = scales::percent, breaks = seq(from=-.3, to=.3, by=.02)) +
    theme_classic() + theme(plot.title=element_text(size=12), legend.position = "none"))


cor(spy.pred.lead.roll.lm.df.graph.lei.24m$pred, spy.pred.lead.roll.lm.df.graph.lei.24m$actual.lead, use = "complete.obs")
cor(spy.pred.lead.roll.lm.df.graph.lei.24m$pred, spy.pred.lead.roll.lm.df.graph.lei.24m$actual.lead, use = "complete.obs")^2
