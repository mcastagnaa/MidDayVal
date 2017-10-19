library(ggplot2)
library(PerformanceAnalytics)
library(reshape2)
library(dplyr)
library(scales)
library(psych)

rm(list = ls())

load("wSet_z.Rda")

chart.CumReturns(wSet.z[, c("pChOpen", "pChClose"),], 
                 wealth.index = T, 
                 legend.loc = "topleft",
                 main = "S&P 500 price index cumulative returns")

chart.CumReturns(wSet.z$relRets, 
                 wealth.index = F, 
                 geometric = F,
                 main = "S&P 500 price index\nrelative returns (Open%-Close%)",
                 ylab = "Cumulative daily")

cov(wSet.z[, c("pChOpen", "pChClose", "relRets")], use = "pairwise")
sqrt(diag(cov(wSet.z[, c("pChOpen", "pChClose", "relRets")], use = "pairwise"))) * sqrt(252) * 100

cor(wSet.z[, c("pChOpen", "pChClose", "relRets")], use = "pairwise")

### Lag close and check correlation with Open ########################################
wSet.z$lagClose <- lag(wSet.z$pChClose, k = 1)
cor(wSet.z[, c("pChOpen", "lagClose", "pChClose")], use = "pairwise")

ggplot(fortify(wSet.z), aes(x=Index)) +
  geom_linerange(aes(ymin = ifelse(Open > Close, Close, Open), 
                     ymax = ifelse(Open <= Close, Close, Open)))

ggplot(fortify(wSet.z)) +
  geom_line(aes(x = Index, y = Open), colour = "grey") +
  geom_line(aes(x = Index, y= Close), colour = "black") +
  theme_bw() + ylab("") + xlab("") + 
  labs(ylab = NULL,
       xlab = NULL,
       title = "S&P 500 - price index", 
       subtitle  = "grey = 0pen, black = Close",
       caption = "https://www.github.com/mcastagnaa/whatever")

ggplot(fortify(wSet.z), aes(intraDay)) + 
  geom_histogram(binwidth = 0.001, colour = "black", fill = "white") + 
  scale_x_continuous(labels = percent) + 
  theme_bw() + xlab("Intraday range - Close vs. Open") + ylab("Observations") +
  ggtitle("S&P 500 - intraday range", subtitle = "price index") +
  stat_function( 
    fun = function(x, mean, sd, n, bw){ 
      dnorm(x = x, mean = mean, sd = sd) * n * bw
    }, 
    args = c(mean = mean(wSet.z$intraDay), 
             sd = sd(wSet.z$intraDay), 
             n = length(wSet.z$intraDay), bw = 0.001),
    colour = "grey")

describe(wSet.z$intraDay*100, IQR = T)
  
densityBy(wSet.z$intraDay*100)
