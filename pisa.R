setwd("C:/export")

pisaTrain <- read.csv("pisa2009train.csv")
pisaTest <- read.csv("pisa2009test.csv")

tapply(pisaTrain$readingScore, pisaTrain$male, mean)

summary(pisaTrain)

pisaTrain = na.omit(pisaTrain)

pisaTest = na.omit(pisaTest)

str(pisaTrain)

pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")

pisaTest$raceeth = relevel(pisaTest$raceeth, "White")

lmScore = lm(readingScore ~ grade + male + raceeth + preschool + expectBachelors + motherHS + motherBachelors + motherWork + fatherHS + fatherBachelors + fatherWork + selfBornUS + motherBornUS + fatherBornUS+ englishAtHome + computerForSchoolwork + read30MinsADay + minutesPerWeekEnglish + studentsInEnglish+schoolHasLibrary+publicSchool+urban+schoolSize, data=pisaTrain)
lmScore = lm(readingScore ~ ., data=pisaTrain)
summary(lmScore)

SSE <- sum(lmScore$residuals^2)
RMSE <- sqrt(SSE/nrow(pisaTrain))
RMSE

predTest <- predict(lmScore, newdata = pisaTest)
max(predTest) - min(predTest)

SSE2 <- sum((predTest - pisaTest$readingScore)^2)
RMSE2 <- sqrt(SSE2/nrow(pisaTest))
RMSE2
SSE2

mean(pisaTrain$readingScore)
SST <- sum((mean(pisaTrain$readingScore)-pisaTest$readingScore)^2)
SST

R2 <- 1 - SSE2/SST
R2
