setwd("c:/export")

trials <- read.csv("clinical_trial.csv", stringsAsFactors = FALSE)

max(nchar(trials$abstract))

nrow(subset(trials, nchar(abstract)==0))

trials$title[which.min(nchar(trials$title))]

library(tm)

corpusTitle <- Corpus(VectorSource(trials$title))
corpusAbstract <- Corpus(VectorSource(trials$abstract))

corpusTitle <- tm_map(corpusTitle, tolower)
corpusAbstract <- tm_map(corpusAbstract, tolower)
corpusTitle <- tm_map(corpusTitle, PlainTextDocument)
corpusAbstract <- tm_map(corpusAbstract, PlainTextDocument)

corpusTitle <- tm_map(corpusTitle, removePunctuation)
corpusAbstract <- tm_map(corpusAbstract, removePunctuation)
corpusTitle <- tm_map(corpusTitle, removeWords, stopwords("english"))
corpusAbstract <- tm_map(corpusAbstract, removeWords, stopwords("english"))
corpusTitle <- tm_map(corpusTitle, stemDocument)
corpusAbstract <- tm_map(corpusAbstract, stemDocument)

dmTitle <- DocumentTermMatrix(corpusTitle)
dmAbstract <- DocumentTermMatrix(corpusAbstract)
dmTitle <- removeSparseTerms(dmTitle, 1-0.05)
dmAbstract <- removeSparseTerms(dmAbstract, 1-0.05)

dtmTitle <- as.data.frame(as.matrix(dmTitle))
dtmAbstract <- as.data.frame(as.matrix(dmAbstract))

sort(colSums(dtmAbstract))

colnames(dtmTitle) = paste0("T", colnames(dtmTitle))

colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))
str(dtm)

dtm = cbind(dtmTitle, dtmAbstract)

dtm$trial <- trials$trial

library(caTools)
set.seed(144)
spl <- sample.split(dtm$trial, SplitRatio = 0.7)
train <- subset(dtm, spl==TRUE)
test <- subset(dtm, spl==FALSE)
table(train$trial)
730/nrow(train)

library(rpart)
library(rpart.plot)

trialCART <- rpart(trial ~ ., data=train, method="class")
prp(trialCART)

predicts <- predict(trialCART, newdata=train)
max(predicts[,2])

table(train$trial, predicts[,2]>0.5)
(631+441)/nrow(train)
(441)/(441+131)
(631)/(631+99)

predicts2 <- predict(trialCart, newdata=test)
table(test$trial, predicts2[,2]>0.8)
(261+162)/nrow(test)

library(ROCR)

pred <- prediction(predicts2[,2], test$trial)
as.numeric(performance(pred, "auc")@y.values)
