setwd("C:/export")

Gerber <- read.csv("gerber.csv")

str(Gerber)

nrow(subset(Gerber, voting == 1))/nrow(Gerber)

nrow(subset(Gerber, voting == 1 & hawthorne == 1))/nrow(Gerber)
nrow(subset(Gerber, voting == 1 & civicduty == 1))/nrow(Gerber)
nrow(subset(Gerber, voting == 1 & neighbors == 1))/nrow(Gerber)
nrow(subset(Gerber, voting == 1 & self == 1))/nrow(Gerber)

Model1 <- lm(voting ~ civicduty + hawthorne + neighbors + self, data=Gerber)
summary(Model1)

predictions <- predict(Model1)
table(Gerber$voting, predictions >0.3)
(134513+51966)/nrow(Gerber)
table(Gerber$voting, predictions >0.5)
(235388)/nrow(Gerber)

library(ROCR)
pred <- prediction(predictions, Gerber$voting)
# Performance function (true positives vs false positives)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize=TRUE)
as.numeric(performance(pred, "auc")@y.values)

library(rpart)
library(rpart.plot)
CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=Gerber)

prp(CARTmodel)
plot(CARTmodel)

CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=Gerber, cp=0.0)
prp(CARTmodel2)
plot(CARTmodel2)

predictions <- predict(CARTmodel2, subset(Gerber, civicduty == 1))

CARTmodel3 = rpart(voting ~ civicduty + hawthorne + self + neighbors + sex, data=Gerber, cp=0.0)
prp(CARTmodel3)

CARTmodel4 <- rpart(voting ~ control, data=Gerber, cp=0.0)
CARTmodel5 <- rpart(voting ~ control + sex, data=Gerber, cp=0.0)

prp(CARTmodel4)
abs(0.2966383-0.340004)
summary(CARTmodel4)

prp(CARTmodel5, digits=6)

abs(0.290456-0.334176)-abs(0.302795-0.345818)

lmModel <- glm(voting ~ sex + control, data=Gerber, family=binomial)
summary(lmModel)
Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(lmModel, newdata=Possibilities, type="response")

abs(0.290456-0.2906107)

LogModel2 = glm(voting ~ sex + control + sex:control, data=Gerber, family="binomial")
summary(LogModel2)
predict(LogModel2, newdata=Possibilities, type="response")
abs(0.290456-0.2904558)


