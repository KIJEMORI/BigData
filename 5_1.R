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
dataset

maxsets <- apply(dataset, 2, max, na.rm = TRUE)
minsets <- apply(dataset, 2, min, na.rm = TRUE)
meanSets <- apply(dataset, 2, mean, na.rm = TRUE)
meanSets

psichical_activity <- scale(dataset, center = minsets, scale = maxsets - minsets)
psichical_activity

dist.psichical_activity <- dist(psichical_activity)

clust.psichical_activity <- hclust(dist.psichical_activity, "ward.D")

# Метод силуэта
library("factoextra")

fviz_nbclust(psichical_activity, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

# Метод локтя
library("cluster")
fviz_nbclust(psichical_activity, kmeans, method = "wss")

# Метод статистика разрыва
gap_stat <- clusGap(psichical_activity, FUN = kmeans, nstart = 5, K.max =5, B = 5)
fviz_gap_stat(gap_stat)

# Алгоритм консенсуса
library ("parameters")

n_clust <- n_clusters(dataset, 
                      package = c("easystats", "nbClust", "mclust"),
                      standardize = FALSE)
plot(n_clust)

# Построение Дендрограммы
plot(clust.psichical_activity)

plot(clust.psichical_activity, cex=0.5)
rect.hclust(clust.psichical_activity, k=2, border="red")

hcd <- as.dendrogram(clust.psichical_activity)
plot(cut(hcd, h = 6)$upper)
plot(cut(hcd, h = 6)$lower[[6]])

# Разделение на группы
count_group<-2
groups <- cutree(clust.psichical_activity, k =count_group)

g <- colMeans(psichical_activity[groups == 1, 1:4])
df <- data.frame(g)
for(i in 2:count_group){
  g <- colMeans(psichical_activity[groups == i, 1:4])
  df <- data.frame(df, g)
}



df
# Постройка столбчатой диаграммы

df1 <- t(df)
df <- t(df1)
barplot(df,ylim=c(0,1), col=c("red","green","blue","yellow"), beside = TRUE, axes = FALSE, main = "Groups of psichical activity")
axis(2, at = 0:2, labels = 0:2)
legend("top", legend = rownames(df), col=c("red","green","blue","yellow"), lwd=10, bty = "n")

boxplot(df, main = "Groups of psichical activity", col=c("blue","yellow"))

# Кластеризация k-means
km.res <- kmeans(psichical_activity, count_group, nstart =10)
fviz_cluster(km.res, psichical_activity)

# Предикторы

pairs(psichical_activity, main = "psichical activity",pch = 19,  cex = 0.8, col = c("#00AFBB", "#E7B800", "#FC4E07"))

library("scatterplot3d")

colors <- c("#00AFBB", "#E7B800", "#FC4E07")
scatterplot3d(psichical_activity, main= "psichical activity",pch = 16, color=colors)
legend(s3d$xyz.convert(7.5, 3, 4.5), legend = c(1:count_group),
       col =  colors, pch = 16)
