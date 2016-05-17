Crime <- read.csv("mvtWeek1.csv")
nrow(Crime)
str(Crime)

max(Crime$ID)
min(Crime$Beat)
table(Crime$Arrest)

nrow(subset(Crime, LocationDescription =="ALLEY"))

Crime$Date[1]

DateConvert <- as.Date(strptime(Crime$Date, "%m/%d/%y %H:%M"))
median(DateConvert)

Crime$Month = months(DateConvert)
Crime$WeekDay = weekdays(DateConvert)

Crime$Date = DateConvert

table(Crime$Month)
table(Crime$WeekDay)

arrested <- subset(Crime, Arrest==TRUE)
str(Crime)
table(arrested$Month)
hist(Crime$Date, breaks=100)

boxplot(Crime$Date ~ Crime$Arrest)

table(format(Crime$Date, "%Y"), Crime$Arrest)
tapply(format(Crime$Date, '"%Y'), Crime$Arrest)

2152/(18517+2152)
1212/(13068+1212)
550/(13542+550)

sort(table(Crime$LocationDescription))

top5 <- subset(Crime, Crime$LocationDescription == "PARKING LOT/GARAGE(NON.RESID.)" | Crime$LocationDescription == "STREET" | Crime$LocationDescription == "ALLEY" | Crime$LocationDescription == "GAS STATION" | Crime$LocationDescription == "DRIVEWAY - RESIDENTIAL" )
nrow(top5)

top5$LocationDescription = factor(top5$LocationDescription)
str(top5)

table(top5$LocationDescription, top5$Arrest, mean)

249/(2509+249)
132/(1543+132)
439/(1672+439)
1603/(13249+1603)
11595/(144969+11595)

gas <- subset(top5, top5$LocationDescription == 'GAS STATION')
max(table(gas$WeekDay))

drive <- subset(top5, top5$LocationDescription == 'DRIVEWAY - RESIDENTIAL')
min(table(drive$WeekDay))

?tapply
