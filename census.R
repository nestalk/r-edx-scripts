setwd("C:/export")

census <- read.csv("census.csv")
str(census)
set.seed(2000)
library(caTools)
split <- sample.split(census$over50k, SplitRatio = 0.6)
train <- subset(census, split== TRUE)
test <- subset(census, split == FALSE)

logModel <- glm(over50k ~ ., data=train, family=binomial)
summary(logModel)
predictions2 <- predict(logModel, newdata=test)
table(test$over50k, predictions > 0.5)
(9351+1515)/nrow(test)
(9351+362)/nrow(test)

library(ROCR)
pred2 <- prediction(predictions2, test$over50k)
as.numeric(performance(pred, "auc")@y.values)

library(rpart)
library(rpart.plot)

CartCensus <- rpart(over50k ~ ., data=train, method="class")
prp(CartCensus)
predictions <- predict(CartCensus, newdata=test, type="class")
table(test$over50k, predictions)
(9243+1596)/nrow(test)

predictions <- predict(CartCensus, newdata=test) 
pred <- prediction(predictions[,2], test$over50k)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize=TRUE)
plot(performance(pred2, "tpr","fpr"), colorize=TRUE)

as.numeric(performance(pred, "auc")@y.values)
as.numeric(performance(pred2, "auc")@y.values)

set.seed(1)
trainSmall <- train[sample(nrow(train), 2000),]

set.seed(1)
forrest <- randomForest(over50k ~ ., data=trainSmall)
predictions <- predict(forrest, newdata=test)
table(test$over50k, predictions )

(9643+852)/nrow(test)

vu <- varUsed(forrest, count=TRUE)
vusorted <- sort(vu, decreasing = FALSE, index.return=TRUE)
dotchart(vusorted$x, names(forrest$forest$xlevels[vusorted$ix]))

varImpPlot(forrest)

set.seed(2)
library(caret)
umFolds = trainControl( method = "cv", number = 10 )
cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002)) 

tr <- train(over50k ~ ., data=train, method="rpart", trControl=umFolds, tuneGrid=cartGrid)
summary(tr)
tr

cartcp <- rpart(over50k ~ . , data=train, cp=0.002, method="class")
predicts <- predict(cartcp, newdata=test, type="class")
table(test$over50k, predicts)
(9178+1838)/nrow(test)
prp(cartcp)
summary(cartcp)
