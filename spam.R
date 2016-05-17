setwd("c:/export")

emails <- read.csv("emails.csv", stringsAsFactors = FALSE)

table(emails$spam)
emails$text[1]
which.min(nchar(emails$text))

library(tm)
corpus <- Corpus(VectorSource(emails$text))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stemDocument)


corpus[[1]]$content

dtm <- DocumentTermMatrix(corpus)
dtm
spdtm <- removeSparseTerms(dtm, 1-0.05)
spdtm

emailsSparse <- as.data.frame(as.matrix(spdtm))
which.max(colSums(emailsSparse))
colnames(emailsSparse) <- make.names(colnames(emailsSparse))
emailsSparse$spam <- emails$spam

ham <- subset(emailsSparse, spam==0)
sort(colSums(ham))
spam <- subset(emailsSparse, spam == 1)
sort(colSums(spam))

emailsSparse$spam <- as.factor(emailsSparse$spam)
set.seed(123)
library(caTools)
spl <- sample.split(emailsSparse$spam, SplitRatio = 0.7)
train <- subset(emailsSparse, spl==TRUE)
test <- subset(emailsSparse, spl==FALSE)
library(rpart)
library(rpart.plot)
spamLog <- glm(spam ~ ., data=train, family=binomial)
spamCART <- rpart(spam ~ ., data=train, method="class")
library(randomForest)
set.seed(123)
spamRF <- randomForest(spam ~ ., data=train)

predictLog <- predict(spamLog, newdata = train, type="response")
predictCART <- predict(spamCART, newdata = train)
predictRF <- predict(spamRF, newdata = train, type="prob")

low <- subset(predictLog, predictLog < 0.0001)
hi <- subset(predictLog, predictLog > 0.9999)
mid <- subset(predictLog, predictLog > 0.0001 & predictLog < 0.9999)

table(predictLog < 0.00001)
table(predictLog > 0.99999)
table(predictLog >= 0.00001 & predictLog <= 0.99999)

summary(spamLog)

prp(spamCART)

table(train$spam, predictLog > 0.5)
(3052+954)/nrow(train)

library(ROCR)
pred <- prediction(predictLog, train$spam)
as.numeric(performance(pred, "auc")@y.values)

table(train$spam, predictCART[,2] > 0.5)
(2885+894)/nrow(train)

predCart <- prediction(predictCART[,2], train$spam)
as.numeric(performance(predCart, "auc")@y.values)


table(train$spam, predictRF[,2] > 0.5)
(3046+958)/nrow(train)

predRf <- prediction(predictRF[,2], train$spam)
as.numeric(performance(predRf, "auc")@y.values)

predictLog2 <- predict(spamLog, newdata = test, type="response")
predictCART2 <- predict(spamCART, newdata = test)
predictRF2 <- predict(spamRF, newdata = test, type="prob")

table(test$spam, predictCART2[,2] > 0.5)
(1228+386)/nrow(test)
predCart2 <- prediction(predictCART2[,2], test$spam)
as.numeric(performance(predCart2, "auc")@y.values)

table(test$spam, predictLog2 > 0.5)
(1257+376)/nrow(test)
predLog2 <- prediction(predictLog2, test$spam)
as.numeric(performance(predLog2, "auc")@y.values)


table(test$spam, predictRF2[,2] > 0.5)
(1290+385)/nrow(test)
predRf2 <- prediction(predictRF2[,2], test$spam)
as.numeric(performance(predRf2, "auc")@y.values)
