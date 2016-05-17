setwd("c:/export")

wiki <- read.csv("wiki.csv", stringsAsFactors = FALSE)
wiki$Vandal <- as.factor(wiki$Vandal)
table(wiki$Vandal)

library(tm)

corpusAdded <- Corpus(VectorSource(wiki$Added))
corpusAdded <- tm_map(corpusAdded, removeWords, stopwords("english"))
corpusAdded <- tm_map(corpusAdded, stemDocument)

dtmAdded <- DocumentTermMatrix(corpusAdded)
dtmAdded

sparseAdded <- removeSparseTerms(dtmAdded, 1-0.003)
sparseAdded

wordsAdded <- as.data.frame(as.matrix(sparseAdded))
colnames(wordsAdded) <- paste("A", colnames(wordsAdded))
str(wordsAdded)

corpusRemoved <- Corpus(VectorSource(wiki$Removed))
corpusRemoved <- tm_map(corpusRemoved, removeWords, stopwords("english"))
corpusRemoved <- tm_map(corpusRemoved, stemDocument)

dtmRemoved <- DocumentTermMatrix(corpusRemoved)
dtmRemoved

sparseRemoved <- removeSparseTerms(dtmRemoved, 1-0.003)
sparseRemoved

wordsRemoved <- as.data.frame(as.matrix(sparseRemoved))
colnames(wordsRemoved) <- paste("R", colnames(wordsRemoved))

wikiWords <- cbind(wordsAdded, wordsRemoved)

wikiWords$Vandal <- wiki$Vandal

library(caTools)
set.seed(123)
split <- sample.split(wikiWords, SplitRatio = 0.7)
wikiTrain <- subset(wikiWords, split == TRUE)
wikiTest <- subset(wikiWords, split == FALSE)

table(wikiTest$Vandal)
(542)/nrow(wikiTest)

library(rpart)
library(rpart.plot)

cartModel <- rpart(Vandal ~ ., data=wikiTrain, method="class")
predictions <- predict(cartModel, newdata=wikiTest, type="class")
table(wikiTest$Vandal, predictions)
(622+14)/nrow(wikiTest)

prp(cartModel)

table(wikiTrain$Vandal, predict(cartModel, newdata=wikiTrain, type="class"))
(1439+49)/nrow(wikiTrain)

wikiWords2 <- wikiWords
wikiWords2$HTTP <- ifelse(grepl("http", wiki$Added, fixed=TRUE),1,0)
table(wikiWords2$HTTP)

wikiTrain2 <- subset(wikiWords2, split==TRUE)
wikiTest2 <- subset(wikiWords2, split==FALSE)

cartModel2 <- rpart(Vandal ~ ., data=wikiTrain2, method="class")
predictions2 <- predict(cartModel2, newdata=wikiTest2, type="class")
table(wikiTest2$Vandal, predictions2)
(2013+214)/nrow(wikiTest2)

wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))

wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))

mean(wikiWords2$NumWordsAdded)

wikiTrain3 <- subset(wikiWords2, split==TRUE)
wikiTest3 <- subset(wikiWords2, split==FALSE)

cartModel3 <- rpart(Vandal ~ ., data=wikiTrain3, method="class")
predictions3 <- predict(cartModel3, newdata=wikiTest3, type="class")
table(wikiTest3$Vandal, predictions3)
(515+250)/nrow(wikiTest3)

wikiWords3 = wikiWords2
wikiWords3$Minor = wiki$Minor

wikiWords3$Loggedin = wiki$Loggedin

wikiTrain4 <- subset(wikiWords3, split==TRUE)
wikiTest4 <- subset(wikiWords3, split==FALSE)

cartModel4 <- rpart(Vandal ~ ., data=wikiTrain4, method="class")
predictions4 <- predict(cartModel4, newdata=wikiTest4, type="class")
table(wikiTest4$Vandal, predictions4)
(598+233)/nrow(wikiTest4)

prp(cartModel4)
