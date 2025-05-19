data <- read.csv("athlete_events.csv")

dataset <- data[data$Sport == "Sailing",]

datasetSportAgeHeightWeight <- data.frame( dataset$Sport,dataset$Age, dataset$Height, dataset$Weight)
datasetSportAgeHeightWeight <- datasetSportAgeHeightWeight[rowSums(is.na(datasetSportAgeHeightWeight)) == 0,]
colnames(datasetSportAgeHeightWeight) = c("Sport","Age", "Height", "Weight")
datasetAgeHeightWeight<-datasetSportAgeHeightWeight[2:length(datasetSportAgeHeightWeight)]

datasetMan <- dataset[dataset$Sex == "M", ]
datasetFemale <- dataset[dataset$Sex == "F", ]

datasetSportManAgeHeightWeight <- data.frame(datasetMan$Sport,datasetMan$Age, datasetMan$Height, datasetMan$Weight)
datasetSportManAgeHeightWeight <- datasetSportManAgeHeightWeight[rowSums(is.na(datasetSportManAgeHeightWeight)) == 0,]
colnames(datasetSportManAgeHeightWeight) = c("Sport","Age", "Height", "Weight")
datasetManAgeHeightWeight<-datasetSportManAgeHeightWeight[2:length(datasetSportManAgeHeightWeight)]

datasetSportFemaleAgeHeightWeight <- data.frame(datasetFemale$Sport,datasetFemale$Age, datasetFemale$Height, datasetFemale$Weight)
datasetSportFemaleAgeHeightWeight <- datasetSportFemaleAgeHeightWeight[rowSums(is.na(datasetSportFemaleAgeHeightWeight)) == 0,]
colnames(datasetSportFemaleAgeHeightWeight) = c("Sport","Age", "Height", "Weight")
datasetFemaleAgeHeightWeight<-datasetSportFemaleAgeHeightWeight[2:length(datasetSportFemaleAgeHeightWeight)]

# Провести дескриптивный анализ
min <- apply(dataset, 2, min, na.rm = TRUE)
max <- apply(dataset, 2, max, na.rm = TRUE)
mean <- apply(datasetAgeHeightWeight, 2, mean, na.rm = TRUE)

minMan <- apply(datasetMan, 2, min, na.rm = TRUE)
maxMan <- apply(datasetMan, 2, max, na.rm = TRUE)
meanMan <- apply(datasetManAgeHeightWeight, 2, mean, na.rm = TRUE)

minFeamle <- apply(datasetFemale, 2, min, na.rm = TRUE)
maxFemale <- apply(datasetFemale, 2, max, na.rm = TRUE)
meanFemale <- apply(datasetFemaleAgeHeightWeight, 2, mean, na.rm = TRUE)

# Проверка на нормальность
help("shapiro.test")

shapiro.test(datasetManAgeHeightWeight$Age)
shapiro.test(datasetManAgeHeightWeight$Height)
shapiro.test(datasetManAgeHeightWeight$Weight)

shapiro.test(datasetFemaleAgeHeightWeight$Age)
shapiro.test(datasetFemaleAgeHeightWeight$Height)
shapiro.test(datasetFemaleAgeHeightWeight$Weight)

qqnorm(datasetManAgeHeightWeight$Age)
qqline(datasetManAgeHeightWeight$Age, col = 2)


library('car')

hist(data$Height)

hist(datasetManAgeHeightWeight$Age)
qqPlot(datasetManAgeHeightWeight$Age)

hist(datasetManAgeHeightWeight$Height)
qqPlot(datasetManAgeHeightWeight$Height)

hist(datasetManAgeHeightWeight$Weight)
qqPlot(datasetManAgeHeightWeight$Weight)

hist(datasetFemaleAgeHeightWeight$Age)
qqPlot(datasetFemaleAgeHeightWeight$Age)

hist(datasetFemaleAgeHeightWeight$Height)
qqPlot(datasetFemaleAgeHeightWeight$Height)

hist(datasetFemaleAgeHeightWeight$Weight)
qqPlot(datasetFemaleAgeHeightWeight$Weight)

# Провести гипотезу о среднем спортсменов

t.test(datasetManAgeHeightWeight$Age, mu = meanMan[1], conf.int = TRUE)
t.test(datasetManAgeHeightWeight$Height, mu = meanMan[2])
t.test(datasetManAgeHeightWeight$Weight, mu = meanMan[3])

wilcox.test(datasetManAgeHeightWeight$Age, mu = meanMan[1], conf.int = TRUE)
wilcox.test(datasetManAgeHeightWeight$Height, mu = meanMan[2], conf.int = TRUE)
wilcox.test(datasetManAgeHeightWeight$Weight, mu = meanMan[3], conf.int = TRUE)

t.test(datasetFemaleAgeHeightWeight$Age, mu = meanFemale[1])
t.test(datasetFemaleAgeHeightWeight$Height, mu = meanFemale[2])
t.test(datasetFemaleAgeHeightWeight$Weight, mu = meanFemale[3])

wilcox.test(datasetFemaleAgeHeightWeight$Age, mu = meanFemale[1], conf.int = TRUE)
wilcox.test(datasetFemaleAgeHeightWeight$Height, mu = meanFemale[2], conf.int = TRUE)
wilcox.test(datasetFemaleAgeHeightWeight$Weight, mu = meanFemale[3], conf.int = TRUE)

summary(datasetManAgeHeightWeight$Age)

#------------------------
# Проверка на нормальность, дисперсию и равсентсве среднего в двух разных видах спорта
dataset2 <- data[data$Sport == "Basketball",]

dataset2SportAgeHeightWeight <- data.frame( dataset2$Sport,dataset2$Age, dataset2$Height, dataset2$Weight)
dataset2SportAgeHeightWeight <- dataset2SportAgeHeightWeight[rowSums(is.na(dataset2SportAgeHeightWeight)) == 0,]
colnames(dataset2SportAgeHeightWeight) = c("Sport","Age", "Height", "Weight")
dataset2AgeHeightWeight<-dataset2SportAgeHeightWeight[2:length(dataset2SportAgeHeightWeight)]

dataset2Man <- dataset2[dataset2$Sex == "M", ]
dataset2Female <- dataset2[dataset2$Sex == "F", ]

dataset2SportManAgeHeightWeight <- data.frame(dataset2Man$Sport,dataset2Man$Age, dataset2Man$Height, dataset2Man$Weight)
dataset2SportManAgeHeightWeight <- dataset2SportManAgeHeightWeight[rowSums(is.na(dataset2SportManAgeHeightWeight)) == 0,]
colnames(dataset2SportManAgeHeightWeight) = c("Sport","Age", "Height", "Weight")
dataset2ManAgeHeightWeight<-dataset2SportManAgeHeightWeight[2:length(dataset2SportManAgeHeightWeight)]

dataset2SportFemaleAgeHeightWeight <- data.frame(dataset2Female$Sport,dataset2Female$Age, dataset2Female$Height, dataset2Female$Weight)
dataset2SportFemaleAgeHeightWeight <- dataset2SportFemaleAgeHeightWeight[rowSums(is.na(dataset2SportFemaleAgeHeightWeight)) == 0,]
colnames(dataset2SportFemaleAgeHeightWeight) = c("Sport","Age", "Height", "Weight")
dataset2FemaleAgeHeightWeight<-dataset2SportFemaleAgeHeightWeight[2:length(dataset2SportFemaleAgeHeightWeight)]

combdatasetFemale <- rbind(datasetSportFemaleAgeHeightWeight, dataset2SportFemaleAgeHeightWeight)

hist(combdatasetFemale$Weight)
bartlett.test(Weight~Sport, data = combdatasetFemale)
t.test(Weight~Sport, data = combdatasetFemale)

t.test(Weight~Sport, data = combdatasetFemale, ver.equal = TRUE)

combdatasetMan <- rbind(datasetSportManAgeHeightWeight, dataset2SportManAgeHeightWeight)

hist(combdatasetMan$Weight)
bartlett.test(Weight~Sport, data = combdatasetMan)
t.test(Weight~Sport, data = combdatasetMan)

t.test(Weight~Sport, data = combdatasetMan, ver.equal = TRUE)

