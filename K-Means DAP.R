#libraries
library(readxl)
library(cluster)
library(factoextra)
library(dplyr)

#cleaning data dan preprocessing data
data <- subset(dataset, Year == 2015)
data <- data[,c(2,5,6,7,20,21,22)]
data <- data.frame(data)
rownames(data) <- data[,1]
data <- data[,-1]
std <- scale(data)
datastd <- data.frame(std)
head(data)
head(datastd)

#K-Means
fviz_nbclust(datastd, kmeans, method = "wss") # metode elbow
fviz_nbclust(datastd, kmeans, method = "silhouette") # metode silhouette
set.seed(123)
gap_stat <- clusGap(datastd, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50) # metode gap statistic
fviz_gap_stat(gap_stat)

#Menjalankan algoritma K-Means
result1 <- kmeans(datastd, centers = 3)
result2 <- kmeans(datastd, centers = 5)
result1

#Nilai euclid koefisien silhouette
value_result1 <- silhouette(result1$cluster, dist(datastd))
fviz_silhouette(value_result1)

#Visualisasi Hasil Klasterisasi
data_cluster <- cbind(data, cluster = result1$cluster)
data_cluster
fviz_cluster(result1, data = data_cluster,
             main = 'Cluster Plot',
             xlab = 'x',
             ylab = 'y',
             geom = "point",
             ggtheme = theme_bw())

#Membuat Statistika Deskriptif 
summary <- data_cluster %>%
  group_by(cluster) %>%
  summarize_all(list(mean = mean, min = min, max = max))

summary
