setwd("c:/export")

letters <- read.csv("letters_ABPR.csv")

str(letters)

letters$isB <- as.factor(letters$letter == "B")

set.seed(1000)
library(caTools)
split <- sample.split(letters$isB, SplitRatio = 0.5)
train <- subset(letters, split == TRUE)
test <- subset(letters, split == FALSE)

table(test$isB, rep(FALSE,1558))
1175/1558

library(rpart)
library(rpart.plot)
CARTb = rpart(isB ~ . - letter, data=train, method="class")
prp(CARTb)
predictions <- predict(CARTb, newdata=test, type="class")
table(test$isB, predictions)
(1118+340)/1558

library(randomForest)
set.seed(1000)
Forrest <- randomForest(isB ~ . -letter, data=train)
predictions <- predict(Forrest, newdata=test)
table(test$isB, predictions)
(1165+374)/1558

str(letters)
set.seed(2000)
split <- sample.split(letters$letter, SplitRatio = 0.5)
train <- subset(letters, split == TRUE)
test <- subset(letters, split == FALSE)

summary(test$letter)

401/1558

CARTletter <- rpart(letter ~ . - isB, data=train, method="class")
predictions <- predict(CARTletter, newdata = test, type="class")
table(test$letter, predictions)
penalty <- matrix(c(1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1), byrow=TRUE, nrow=4)
sum(as.matrix(table(test$letter,predictions))*penalty)/1558

set.seed(1000)
ForrestLetter <- randomForest(letter ~ . -letter, data=train)
predictions <- predict(ForrestLetter, newdata=test)
sum(as.matrix(table(test$letter,predictions))*penalty)/1558
