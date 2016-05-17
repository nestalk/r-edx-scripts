setwd("c:/export")

stocks <- read.csv("StocksCluster.csv")

str(stocks)

table(stocks$PositiveDec)
(6424)/nrow(stocks)

sort(cor(stocks))

summary(stocks)

library(caTools)
set.seed(144)
spl = sample.split(stocks$PositiveDec, SplitRatio = 0.7)
stocksTrain = subset(stocks, spl == TRUE)
stocksTest = subset(stocks, spl == FALSE)

StocksModel <- glm(PositiveDec ~ ., data=stocksTrain, family = binomial)
predictStocks <- predict(StocksModel, type="response")
table(stocksTrain$PositiveDec, predictStocks > 0.5)
(990+3640)/nrow(stocksTrain)
summary(StocksModel)

predictTest <- predict(StocksModel, newdata=stocksTest, type="response")
table(stocksTest$PositiveDec, predictTest > 0.5)
(417+1553)/nrow(stocksTest)

table(stocksTest$PositiveDec)
1897/nrow(stocksTest)

limitedTrain = stocksTrain
limitedTrain$PositiveDec = NULL

limitedTest = stocksTest
limitedTest$PositiveDec = NULL

library(caret)
preproc = preProcess(limitedTrain)
normTrain = predict(preproc, limitedTrain)
normTest = predict(preproc, limitedTest)

summary(normTrain)
summary(normTest)

set.seed(144)
km = kmeans(normTrain, centers = 3)
str(km)

library(flexclust)
km.kcca = as.kcca(km, normTrain)
clusterTrain = predict(km.kcca)
clusterTest = predict(km.kcca, newdata=normTest)
table(clusterTest)

stocksTrain1 <- subset(stocksTrain, clusterTrain == 1)
stocksTrain2 <- subset(stocksTrain, clusterTrain == 2)
stocksTrain3 <- subset(stocksTrain, clusterTrain == 3)

stocksTest1 <- subset(stocksTest, clusterTest == 1)
stocksTest2 <- subset(stocksTest, clusterTest == 2)
stocksTest3 <- subset(stocksTest, clusterTest == 3)

mean(stocksTrain1$PositiveDec)
mean(stocksTrain2$PositiveDec)
mean(stocksTrain3$PositiveDec)

StocksModel1 <- glm(PositiveDec ~ ., data=stocksTrain1, family=binomial)
StocksModel2 <- glm(PositiveDec ~ ., data=stocksTrain2, family=binomial)
StocksModel3 <- glm(PositiveDec ~ ., data=stocksTrain3, family=binomial)

summary(StocksModel1)
summary(StocksModel2)
summary(StocksModel3)

PredictTest1 <- predict(StocksModel1, newdata=stocksTest1, type="response")
PredictTest2 <- predict(StocksModel2, newdata=stocksTest2, type="response")
PredictTest3 <- predict(StocksModel3, newdata=stocksTest3, type="response")

table(stocksTest1$PositiveDec, PredictTest1 > 0.5)
table(stocksTest2$PositiveDec, PredictTest2 > 0.5)
table(stocksTest3$PositiveDec, PredictTest3 > 0.5)

(30+774)/nrow(stocksTest1)
(388+757)/nrow(stocksTest2)
(49+13)/nrow(stocksTest3)

AllPredictions = c(PredictTest1, PredictTest2, PredictTest3)
AllOutcomes = c(stocksTest1$PositiveDec, stocksTest2$PositiveDec, stocksTest3$PositiveDec)

table(AllOutcomes, AllPredictions > 0.5)
(467+1544)/3474
