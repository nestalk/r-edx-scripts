setwd("C:/export")

FluTrain <- read.csv("FluTrain.csv")
str(FluTrain)

max(FluTrain$ILI)
max(FluTrain$Queries)

FluTrain$Week[match(max(FluTrain$ILI), FluTrain$ILI)]
FluTrain$Week[match(max(FluTrain$Queries), FluTrain$Queries)]

hist(FluTrain$ILI)

plot(log(FluTrain$ILI), FluTrain$Queries)

FluTrend1 <- lm(log(ILI) ~ Queries, data=FluTrain)
summary(FluTrend1)

cor(log(FluTrain$ILI), FluTrain$Queries)^2
log(1/cor(log(FluTrain$ILI), FluTrain$Queries))
exp(-0.5*cor(log(FluTrain$ILI), FluTrain$Queries))

FluTest <- read.csv("FluTest.csv")

PredTest1 = exp(predict(FluTrend1, newdata=FluTest))
?which
PredTest1

FluTest$Predict <- PredTest1
which(FluTest$Week == "2012-03-11 - 2012-03-17")
FluTest

sum((FluTest$ILI[11] - PredTest1[11])/FluTest$ILI[11])

SSE <- sum((FluTest$ILI - PredTest1)^2)
RMSE <- sqrt(SSE/nrow(FluTest))
RMSE

install.packages("zoo")
library(zoo)

ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)

FluTrain$ILILag2 = coredata(ILILag2)

summary(ILILag2)

plot(log(FluTrain$ILILag2), log(FluTrain$ILI))

FluTrend2 <- lm(log(ILI) ~ Queries + log(ILILag2), data=FluTrain)
summary(FluTrend2)
summary(FluTrend1)

ILILag2 = lag(zoo(FluTest$ILI), -2, na.pad=TRUE)

FluTest$ILILag2 = coredata(ILILag2)
summary(FluTest)

nrow(FluTrain)
FluTrain$Week[417]

FluTest$ILILag2[1] = FluTrain$ILI[416]
FluTest$ILILag2[2] = FluTrain$ILI[417]
head(FluTest)
tail(FluTrain)

PredTest2 <- exp(predict(FluTrend2, newdata = FluTest))
SSE2 <- sum((FluTest$ILI - PredTest2)^2)
RMSE2 <- sqrt(SSE2/nrow(FluTest))
RMSE2
