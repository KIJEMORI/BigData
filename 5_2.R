PA <- read.csv("BRFSS Physical Acticity.csv")
location <- read.csv("BRFSS location_id KEY.csv")

locations_id <- PA[,2]
locations_id <- as.numeric(levels(factor(locations_id)))
locations_id
dataset_st <- PA[c(2,4:length(PA))]

# Замена пропущенных данных на среднее значение по каждой локации
dataset <- data.frame()
for (i in 1:length(locations_id)){
  df <- subset(dataset_st, dataset_st$location_id == locations_id[i])
  for (i in (1:length(df))){
    df[,i][is.na(df[,i])] = mean(df[,i], na.rm = TRUE)
  }
  dataset<-rbind(dataset, df)
}

# Оставим только положительные ответы в опросах(положительные минус отрицательные)
dataset
dataset <- dataset[rowSums(is.na(dataset)) == 0,]
dataset <- dataset[2:length(dataset)]
dataset <- dataset[c(1:(length(dataset)/2))*2]
dataset <- dataset[c(1:(length(dataset)/2))*2-1] - dataset[c(1:(length(dataset)/2))*2]

dataset
maxsets <- apply(dataset, 2, max, na.rm = TRUE)
minsets <- apply(dataset, 2, min, na.rm = TRUE)

for (i in (1:length(rownames(dataset)))){
  dataset[i,] = dataset[i,] + abs(minsets)
}


maxsets <- apply(dataset, 2, max, na.rm = TRUE)
minsets <- apply(dataset, 2, min, na.rm = TRUE)
meanSets <- apply(dataset, 2, mean, na.rm = TRUE)
meanSets

psichical_activity <- scale(dataset, center = minsets, scale = maxsets - minsets)
psichical_activity


dist.psichical_activity <- dist(psichical_activity)

clust.psichical_activity <- hclust(dist.psichical_activity, "ward.D")

# Разделение на группы
count_group<-2

groups <- cutree(clust.psichical_activity, k =count_group)
groups
g <- colMeans(dataset[groups == 1, 1:4])
df <- data.frame(g)
for(i in 2:count_group){
  g <- colMeans(dataset[groups == i, 1:4])
  df <- data.frame(df, g)
}

# Наивный Байесовский подход
library("klaR")

my_data <- dataset
answers <- factor(groups)
my_data <- data.frame(my_data, answers)

train.ind <- sample(1:nrow(dataset), ceiling(nrow(dataset)*2/3), replace=FALSE)
train.ind

train_data <- my_data[train.ind,]
train_data

naive_phis <- NaiveBayes(train_data$answers ~ ., data = train_data)
naive_phis$tables

layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
plot(naive_phis,lwd = 2, legendplot=TRUE)
layout(1)

test_data <- my_data[-train.ind,]
test_data <- test_data[,-ncol(test_data)]
test_data
answer_test <- my_data[-train.ind,]$answers
pred <- predict(naive_phis, test_data)

table(Факт = answer_test, Прогноз = pred$class)

Acc <- mean(pred$class == answer_test)
Acc

# Деревья решений
library("party")

set.seed(1234)

nrow(train_data)
nrow(test_data)
nrow(my_data)

colnames(dataset)

myFormula <- answers ~ PAINDX2_yes_sample + PASTAE2_yes_sample + PASTRNG_yes_sample + TOTINDA_yes_sample
ctree_phis <- ctree(myFormula, data = train_data) 
pred_ctree <- predict(ctree_phis, test_data)
pred_ctree
table(Факт = answer_test, Прогноз = pred_ctree)

plot(ctree_phis,
     inner_panel=node_inner,
     ip_args=list(
       beside=TRUE,
       abbreviate = TRUE, 
       id = FALSE)
)

Acc <- mean(pred_ctree == answer_test)
Acc



# Случайный лес
# install.packages("randomForest")

library("randomForest")
# 1
rf <- randomForest(train_data$answers ~. , data = train_data, ntree = 100, proximity= TRUE)
pred_rf <- predict(rf, test_data)
table(Факт = answer_test, Прогноз = pred_rf)

Acc <- mean(pred_rf == answer_test)
Acc

print(rf)

# 2
cf <- cforest(train_data$answers ~. , data = train_data, control = cforest_unbiased(mtry=2,ntree=100))
pred_cf <- predict(cf, test_data)
pred_cf
table(Факт = answer_test, Прогноз = pred_cf)

Acc <- mean(pred_cf == answer_test)
Acc

print(rf)

