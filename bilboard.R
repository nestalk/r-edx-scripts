setwd("c:/export")
songs <- read.csv("songs.csv")
str(songs)
nrow(subset(songs, year == "2010"))
nrow(subset(songs, artistname == "Michael Jackson"))
subset(songs, artistname == "Michael Jackson" & Top10 == 1)$songtitle

table(as.factor(songs$timesignature))

songs[which.max(songs$tempo),]

SongsTrain <- subset(songs, year < 2010)
SongsTest <- subset(songs, year == 2010)
nrow(SongsTrain)

nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]

SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]
str(SongsTrain)

Model1 = glm(Top10 ~ ., data=SongsTrain, family=binomial)
summary(Model1)

cor(SongsTrain$loudness, SongsTrain$energy)

Model2 = glm(Top10 ~ . - loudness, data=SongsTrain, family=binomial)
summary(Model2)

Model3 = glm(Top10 ~ . - energy, data=SongsTrain, family=binomial)
summary(Model3)

prediction <- predict(Model3, newdata=SongsTest, type="response")
table(SongsTest$Top10, prediction > 0.45)

(309+19)/nrow(SongsTest)
(309+5)/nrow(SongsTest)

309/(309+5)
19/(19+40)
