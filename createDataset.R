library(dplyr)
library(PerformanceAnalytics)

rm(list = ls())
openTime <- "14:30"
closeTime <- "20:00"

load("rawData.Rda")

openData <- rawData[rawData$Time == openTime, c("Date", "Close")]
closeData <- rawData[rawData$Time == closeTime, c("Date", "Close")]

workedSet <- merge(openData, closeData, by = "Date", all = T)
workedSet <- rename(workedSet,
                    Open = Close.x,
                    Close = Close.y)
workedSet <- workedSet[order(workedSet$Date),]

workedSet$Open <- ifelse(is.na(workedSet$Open), workedSet$Close, workedSet$Open)
workedSet$Close <- ifelse(is.na(workedSet$Close), workedSet$Open, workedSet$Close)

workedSet$intraDay <- workedSet$Close/workedSet$Open - 1

workedSet$chOpen <- c(NA, workedSet$Open[-1] - workedSet$Open[-length(workedSet$Open)])
workedSet$chClose <- c(NA, workedSet$Close[-1] - workedSet$Close[-length(workedSet$Close)])

workedSet$pChOpen <- c(NA, workedSet$Open[-1]/workedSet$Open[-length(workedSet$Open)]-1)
workedSet$pChClose <- c(NA, workedSet$Close[-1]/workedSet$Close[-length(workedSet$Close)]-1)

workedSet$relRets <- workedSet$pChOpen - workedSet$pChClose

wSet.z <- xts(workedSet[,-1], order.by = workedSet$Date)

save(wSet.z, file = "wSet_z.Rda")