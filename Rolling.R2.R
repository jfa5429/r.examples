#ROLLING R^2
#SPURIOUS CORRELATION IN RANDOM NUMBERS

asset.1 <- rnorm(1000, mean = 0, sd = 1)
asset.2 <- rnorm(1000, mean = 0, sd = 1)
data.df <- data.frame(asset.1, asset.2)
data.df$day <- seq(1, nrow(data.df), by = 1)

(nr <- nrow(data.df))
r2.fcn <- function(x) summary(lm(asset.1 ~ asset.2, data.df, subset = x))$adj.r.squared
r2.output <- rollapplyr(1:nr, 20, r2.fcn, fill = NA)  #RANDOM 20 DAY ROLLING R^2
r2.output.df <- data.frame(r2.output)
ggplot(data.df, aes(day, r2.output)) + geom_line() + geom_hline(yintercept = .05, color = "red")
