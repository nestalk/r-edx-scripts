setwd("c:/export")

airlines <- read.csv("AirlinesCluster.csv")
str(airlines)
summary(airlines)

library(caret)

preproc = preProcess(airlines)

airlinesNorm = predict(preproc, airlines)

summary(airlinesNorm)

distances <- dist(airlinesNorm, method="euclidean")
airHistCluster <- hclust(distances, method = "ward.D")
plot(airHistCluster)
rect.hclust(airHistCluster, k = 2, border = "red")
rect.hclust(airHistCluster, k = 3, border = "blue")
rect.hclust(airHistCluster, k = 6, border = "green")
rect.hclust(airHistCluster, k = 7, border = "purple")

airHistTree <- cutree(airHistCluster, k = 5)
table(airHistTree)

tapply(airlines$Balance, airHistTree, mean)
tapply(airlines$QualMiles, airHistTree, mean)
tapply(airlines$BonusMiles, airHistTree, mean)
tapply(airlines$BonusTrans, airHistTree, mean)
tapply(airlines$FlightMiles, airHistTree, mean)
tapply(airlines$FlightTrans, airHistTree, mean)
tapply(airlines$DaysSinceEnroll, airHistTree, mean)

set.seed(88)
airKmeans = kmeans(airlinesNorm, centers = 5, iter.max = 1000)
str(airKmeans)
airKmeansCluster = airKmeans$cluster

table(airKmeans$centers)
