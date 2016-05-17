setwd("c:/export")

climate <- read.csv("climate_change.csv")
climateTrain <- subset(climate, Year <= 2006)
climateTesting <- subset(climate, Year > 2006)

str(climate)

model1 <- lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data = climateTrain)
summary(model1)

cor(climateTrain)


model2 <- lm(Temp ~ MEI + N2O + TSI + Aerosols, data = climateTrain)
summary(model2)

model3 <- step(model1)
?step
summary(model3)

result <- predict(model3, climateTesting)

SSE<-sum((result - climateTesting$Temp)^2)
SSE
SST <- sum((mean(climateTrain$Temp) - climateTesting$Temp)^2)
SST
R2 <- 1 - SSE/SST
R2
