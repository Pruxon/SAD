require(ggplot2)
require(ggpubr)

tauronPeSeptember = read.csv('data/TAURONPE.prn')

openFixings = tauronPeSeptember[tauronPeSeptember$TIME == 90001,]

afterOpenFixings = tauronPeSeptember[tauronPeSeptember$TIME > 90001,]
duringDayTrading = afterOpenFixings[afterOpenFixings$TIME <= 165500,]
duringDayTrading = aggregate(x = duringDayTrading$AMOUNT, by = list(duringDayTrading$DATE), FUN = sum)
duringDayTrading$AMOUNT = duringDayTrading$x

closeFixings = tauronPeSeptember[tauronPeSeptember$TIME >= 170000,]
closeFixings = aggregate(x = closeFixings$AMOUNT, by = list(closeFixings$DATE), FUN = sum)
closeFixings$AMOUNT = closeFixings$x


daysNumber = length(openFixings$DATE)
septemberSum = openFixings$AMOUNT + duringDayTrading$AMOUNT + closeFixings$AMOUNT

#rozkład w zależności od fazy
phases = data.frame(openFixings$DATE, openFixings$AMOUNT/septemberSum, duringDayTrading$AMOUNT/septemberSum, closeFixings$AMOUNT/septemberSum)
colnames(phases2) = c("date", "open", "during_day", "close")

ggplot(phases, aes(date)) + geom_line(aes(y = open, color = "open fixing")) +
  geom_line(aes(y = close, color = "close fixing")) +
  geom_line(aes(y = during_day, color = "during day trading")) +
  labs(title = "Distribution of transactions in days depending on trading phase", x = "Data", y = "Percentage share of transactions")

#pudełkowy
phases2 = data.frame(amount = c(openFixings$AMOUNT/septemberSum, duringDayTrading$AMOUNT/septemberSum, closeFixings$AMOUNT/septemberSum),
                   phase = c(rep("open", daysNumber), rep("during day", daysNumber), rep("close", daysNumber)),
                   data = c(openFixings$DATE, openFixings$DATE, openFixings$DATE))
ggboxplot(phases2, x = "phase", y = "amount", color = "phase", palette = "npg") + labs(title="Distribution of transacrions in days depending onn the trading phase", x="Phase", y="Percentage share in transactions")


#b

selectedDay = tauronPeSeptember[tauronPeSeptember$DATE == 20200917,]
selectedData = selectedDay[selectedDay$TIME > 100000,]
selectedData = selectedData[selectedData$TIME <= 120000,]

chosenTime = selectedData
chosenTime$MINUTE = ((selectedData$TIME - selectedData$TIME%%100)%%10000)/100
chosenTime$HOUR = (selectedData$TIME - selectedData$TIME%%10000)/10000
chosenTime$AMOUNT = selectedData$AMOUNT



aggregatedData = rep(0, 120)
minuteIterator = as.integer(0)
hourIterator = as.integer(10)
dataIterator = as.integer(1)
for (i in 1:length(chosenTime$MINUTE)) {
  if ((chosenTime$MINUTE[i] == minuteIterator) && (chosenTime$HOUR[i] == hourIterator)){
    aggregatedData[dataIterator] = aggregatedData[dataIterator] + chosenTime$AMOUNT[i]
  } else {
    while (chosenTime$MINUTE[i] != minuteIterator || chosenTime$HOUR[i] != hourIterator) {
      minuteIterator = minuteIterator + 1
      if (minuteIterator == 60) {
        hourIterator = hourIterator + 1
        minuteIterator = 0
      }
      dataIterator = dataIterator + 1
    }
    aggregatedData[dataIterator] = aggregatedData[dataIterator] + chosenTime$AMOUNT[i]
  }
}

minutes = rep(0, 120)
for (i in 1:120) {
  minutes[i] = i
}
outputData <- data.frame(aggregatedData, minutes)

ggplot(outputData) + geom_point(aes(x = minutes, y = aggregatedData/10000)) + labs(title="The occurrence of transactions over time", x="Time", y="Number of transactions in one minute [10^4]")


lambda = mean(outputData$aggregatedData/10000)
hist(outputData$aggregatedData/10000, breaks = 10, ylim = c(0, 0.6), prob = T, xlab = "Number of transactions in one minute[10^4]",  ylab = "Probability of occurance",
  main = "Comparison with TauronPe model")
  curve(dpois(x, lambda = lambda), add = T, col = "blue", 0, 100)
  grid()
