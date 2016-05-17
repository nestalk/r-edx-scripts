setwd("C:/export")

parole <- read.csv("parole.csv")
nrow(parole)
table(parole$violator)
str(parole)

parole$crime <- as.factor(parole$crime)
parole$state <- as.factor(parole$state)

summary(parole)

set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio= 0.7)
train <- subset(parole, split==TRUE)
test <- subset(parole, split==FALSE)

Model1 <- glm(violator ~ ., data=train, family=binomial)
summary(Model1)

exp(1.6119919)

1/(1+exp(-(-4.2411574+
             (0.3869904*1)+
             (0.8867192*1)+
             (-0.0001756*50)+
             (-0.1238867*3)+
             (0.0802954*12)+
             (0.6837143*1))))
exp(
  -4.2411574+
      (0.3869904*1)+
      (0.8867192*1)+
      (-0.0001756*50)+
      (-0.1238867*3)+
      (0.0802954*12)+
      (0.6837143*1)
    )

parole[1,]
parole$state

predictions <- predict(Model1, newdata = test, type="response")
max(predictions)

table(test$violator, predictions > 0.5)
12/(11+12)
167/(167+12)
(167+12)/nrow(test)
(167+12)/nrow(test)

library(ROCR)
pred <- prediction(predictions, test$violator)
# Performance function (true positives vs false positives)
perf <- performance(pred, "tpr", "fpr")
# Plot function
plot(perf)
# Plot with colour and labels
plot(perf, colorize=TRUE, print.cutoffs.at=seq(0,1,bi=0.1), text.adj=c(-0.2,1.7))

as.numeric(performance(pred, "auc")@y.values)
