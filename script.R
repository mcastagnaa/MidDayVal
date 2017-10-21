library(ggplot2)
library(PerformanceAnalytics)
library(reshape2)
library(dplyr)
library(scales)
library(psych)
#library(tidyquant)

rm(list = ls())

load("wSet_z.Rda")

### Cumulative relative returns #####################################################
png('cumRelRets.png')

chart.CumReturns(wSet.z$relRets, 
                 wealth.index = F, 
                 geometric = F,
                 main = "S&P 500 price index\nrelative returns (Open% - Close%)",
                 ylab = "Cumulative daily")
dev.off()

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

### Cumulative returns ###########################################################
ggplot(fortify(wSet.z)) +
  geom_line(aes(x = Index, y = Open), colour = "grey") +
  geom_line(aes(x = Index, y= Close), colour = "black") +
  theme_bw() + ylab("") + xlab("") + 
  labs(ylab = NULL,
       xlab = NULL,
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

### Descriptive stats ################################################################
describe(wSet.z$intraDay*100, IQR = T)
densityBy(wSet.z$intraDay*100)

### Weekly stats #####################################################################

