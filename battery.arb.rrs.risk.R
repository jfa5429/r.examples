#SETTING ARB TO ZERO, RRS VS SASM WITH ARB CALL AND SASM CALL
#put + sasm call + arb call vs AS
optionValPlot <- function(n, size, len, med, var, sasm, arb.strike, AS, out.prob) {
  n <- n
  size <- size
  set.seed(0)
  day <- seq(from = 1, to = n, by = 1)
  outage01 <- replicate(n = n, sum(rbinom(n = size, size =  1, prob = out.prob)))
  replace.cost <- replicate(n = n, rlnorm(1, log(med), log(var)))
  lmp <- rlnorm(10000, log(67), log(2.5)) #$100 mean
  outagePut <- data.frame(day, outage01, replace.cost, lmp)
  put.vec2 <- vector(mode = "numeric", length = length((len+1)))
  for(i in 0:len) {
    a <- ifelse(outagePut$outage01 > i-1, outagePut$replace.cost, 0)
    b <- ifelse(outagePut$lmp > arb.strike, (outagePut$lmp - arb.strike), 0)
    put.vec2[i] <- mean(a) + mean(b)
  }
  (put.barplot <- barplot(put.vec2, col = "red", main = "Expected value of incremental daily battery\noutage cover put option + SASM call option + Arb call option",
                          xlab = "inverter", ylab = "Put Value MW/day", xaxt = "n", yaxt = "n"))
  axis(1, at = seq(0, len, by = 1), las=2)
  axis(2, at=axTicks(2), labels=sprintf("$%s", axTicks(2)))
  #text(put.barplot, 0, round(put.vec2, 2),cex=0.6,pos=3)
  abline(h=AS, lwd=2)
}
optionValPlot(n = 10000, size = 50, len = 50, med = 430, var = 1.75, sasm = 0, arb.strike = 500, AS = 400, out.prob = .09)