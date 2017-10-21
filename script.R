library(ggplot2)
library(PerformanceAnalytics)
library(reshape2)
library(dplyr)
library(scales)
library(psych)

rm(list = ls())

load("wSet_z.Rda")

### Cumulative relative returns #####################################################
chart.CumReturns(wSet.z$relRets, 
                 wealth.index = F, 
                 geometric = F,
                 main = "S&P 500 price index\nrelative returns (Open% - Close%)",
                 ylab = "Cumulative daily")

### Cumulative relative returns (ggplot2) ###########################################
wSet.z$cumRelRets <- c(NA, cumsum(wSet.z$relRets[-1]))

ggplot(fortify(wSet.z[-1,])) +
  geom_line(aes(x = Index, y = cumRelRets), colour = "black") +
  theme_bw() +
  labs(y = "Cumulative daily",
       x = NULL,
       title = "S&P 500 price index 'relative returns'", 
       subtitle  = "Open % - Close%",
       caption = "https://github.com/mcastagnaa/MidDayVal")

ggsave("cumRelRets.png", device = "png")

### Covariance and correlations #####################################################
covMat <- cov(wSet.z[, c("pChOpen", "pChClose", "relRets")], use = "pairwise")
print(sqrt(diag(covMat)) * sqrt(252) * 100)

corMat <- cor(wSet.z[, c("pChOpen", "pChClose", "relRets")], use = "pairwise")
print(corMat)

### Lag close and check correlation with Open #######################################
wSet.z$lagClose <- lag(wSet.z$pChClose, k = 1)
cor(wSet.z[, c("pChOpen", "lagClose", "pChClose")], use = "pairwise")

### Min/Max range ###################################################################
ggplot(fortify(wSet.z), aes(x=Index)) +
  geom_linerange(aes(ymin = ifelse(Open > Close, Close, Open), 
                     ymax = ifelse(Open <= Close, Close, Open)))

### Cumulative returns ##############################################################
ggplot(fortify(wSet.z)) +
  geom_line(aes(x = Index, y = Open), colour = "grey") +
  geom_line(aes(x = Index, y= Close), colour = "black") +
  theme_bw() + 
  labs(y = NULL,
       x = NULL,
       title = "S&P 500 - price index", 
       subtitle  = "grey = 0pen, black = Close",
       caption = "https://github.com/mcastagnaa/MidDayVal")

ggsave("TSreturns.png", device = "png")

### Distribution of intraday returns ################################################
ggplot(fortify(wSet.z), aes(intraDay)) + 
  geom_histogram(binwidth = 0.001, colour = "black", fill = "white") + 
  scale_x_continuous(labels = percent) +
  theme_bw() +
  stat_function( 
    fun = function(x, mean, sd, n, bw){ 
      dnorm(x = x, mean = mean, sd = sd) * n * bw
    }, 
    args = c(mean = mean(wSet.z$intraDay), 
             sd = sd(wSet.z$intraDay), 
             n = length(wSet.z$intraDay), bw = 0.001),
    colour = "grey") + 
  labs(y = "# of observations",
       x = "Intraday range - Close vs. Open",
       title = "S&P 500 price index- intraday range", 
       subtitle  = "vs. normal distribution with same mean/sd",
       caption = "https://github.com/mcastagnaa/MidDayVal")

ggsave("Distribution.png", device = "png")

### Descriptive stats #############################################################
print(describe(wSet.z$intraDay*100, IQR = T))
densityBy(wSet.z$intraDay*100)

### Weekly stats #####################################################################
wSet.z.w <- to.weekly(wSet.z[, c("Open", "Close")], name = NULL, OHLC = F)
wSet.z.w$pChOpen <- wSet.z.w$Open/lag(wSet.z.w$Open) - 1
wSet.z.w$pChClose <- wSet.z.w$Close/lag(wSet.z.w$Close) - 1
wSet.z.w$relRets <- wSet.z.w$pChOpen - wSet.z.w$pChClose

chart.CumReturns(wSet.z.w$relRets, 
                 wealth.index = F, 
                 geometric = F,
                 main = "S&P 500 price index\nrelative returns (Open% - Close%)",
                 ylab = "Cumulative weekly")

covMatw <- cov(wSet.z.w[, c("pChOpen", "pChClose", "relRets")], use = "pairwise")
print(sqrt(diag(covMatw)) * sqrt(52) * 100)

### Monthly stats ###################################################################
wSet.z.m <- to.monthly(wSet.z[, c("Open", "Close")], name = NULL, OHLC = F)
wSet.z.m$pChOpen <- wSet.z.m$Open/lag(wSet.z.m$Open) - 1
wSet.z.m$pChClose <- wSet.z.m$Close/lag(wSet.z.m$Close) - 1
wSet.z.m$relRets <- wSet.z.m$pChOpen - wSet.z.m$pChClose

chart.CumReturns(wSet.z.m$relRets, 
                 wealth.index = F, 
                 geometric = F,
                 main = "S&P 500 price index\nrelative returns (Open% - Close%)",
                 ylab = "Cumulative monthly")

covMatm <- cov(wSet.z.m[, c("pChOpen", "pChClose", "relRets")], use = "pairwise")
print(sqrt(diag(covMatw)) * sqrt(12) * 100)
