setwd("c:/export")

kos <- read.csv("dailykos.csv")
str(kos)

distances <- dist(kos, method="euclidean")
histCluster <- hclust(distances, method = "ward.D")
plot(histCluster)
rect.hclust(histCluster, k = 2, border = "red")
rect.hclust(histCluster, k = 3, border = "blue")
rect.hclust(histCluster, k = 5, border = "green")
rect.hclust(histCluster, k = 6, border = "purple")

kosClusterGroup <- cutree(histCluster, k = 7)
str(kosClusterGroup)

table(kosClusterGroup)

tail(sort(colMeans(subset(kos, kosClusterGroup == 1))))
tail(sort(colMeans(subset(kos, kosClusterGroup == 2))))
tail(sort(colMeans(subset(kos, kosClusterGroup == 3))))
tail(sort(colMeans(subset(kos, kosClusterGroup == 4))))
tail(sort(colMeans(subset(kos, kosClusterGroup == 5))))
tail(sort(colMeans(subset(kos, kosClusterGroup == 6))))
tail(sort(colMeans(subset(kos, kosClusterGroup == 7))))

set.seed(1000)
kosKMeans = kmeans(kos, centers = 7)
str(kosKMeans)
kosClusters = kosKMeans$cluster

tail(sort(colMeans(subset(kos, kosClusters == 1))))
tail(sort(colMeans(subset(kos, kosClusters == 2))))
tail(sort(colMeans(subset(kos, kosClusters == 3))))
tail(sort(colMeans(subset(kos, kosClusters == 4))))
tail(sort(colMeans(subset(kos, kosClusters == 5))))
tail(sort(colMeans(subset(kos, kosClusters == 6))))
tail(sort(colMeans(subset(kos, kosClusters == 7))))


table(kosClusterGroup, kosClusters)
