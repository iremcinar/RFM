install.packages("Hmisc")
install.packages("cowplot")
install.packages("WVPlots")
install.packages("plotly")
install.packages("tidyverse")
install.packages("factoextra")
install.packages("scales")
install.packages("funModeling")
install.packages("fastmap")
install.packages("pheatmap")
install.packages("RColorBrewer")
install.packages("corrplot")
install.packages("gridExtra")

library(ggplot2)
library(dplyr)
library(Hmisc)
library(cowplot)

data <-read.table("/Users/ilaydaefe/Desktop/BuyukVerıProje/rfm_datamart.csv", header=T, sep = ",")
head(data)
describe(data)
m <- as.matrix(data)
#Görselleştirme
library(plotly)
library(ggplot2)
#m <- as.data.frame(m)  ##aşağısı çalışmazsa geri dön bunu çalıştır!
#MonetaryValue 
set.seed(1)    
x1 <- m$MonetaryValue
group_labels = c('MonetaryValue')  
df1 <- data.frame(x1, group_labels[1])  
colnames(df1) <- c('x', 'Group') 
df <- rbind(df1) 
colnames(df) <- c('x', 'Group') 
gg <- ggplot(data = df ) +  
  geom_density(aes(x=x, color=Group)) + geom_rug(aes(x=x, color=Group)) + 
  ylab("") + 
  xlab("")
ggplotly(gg)%>% 
  layout(plot_bgcolor='#e5ecf6',   
         xaxis = list(   
           title='MonetaryValue', 
           zerolinecolor = '#ffff',   
           zerolinewidth = 2,   
           gridcolor = 'ffff'),   
         yaxis = list(   
           title='Yoğunluk', 
           zerolinecolor = '#ffff',   
           zerolinewidth = 2,   
           gridcolor = 'ffff'),
         title = 'MonetaryValue') 

#Recency
set.seed(1)    
x1 <- m$Recency
group_labels = c('Recency')  
df1 <- data.frame(x1, group_labels[1])  
colnames(df1) <- c('x', 'Recency') 
df <- rbind(df1) 
colnames(df) <- c('x', 'Group') 
gg <- ggplot(data = df ) +  
  geom_density(aes(x=x, color=Group)) + geom_rug(aes(x=x, color=Group)) + 
  ylab("") + 
  xlab("")
ggplotly(gg)%>% 
  layout(plot_bgcolor='#e5ecf6',   
         xaxis = list(   
           title='Recency', 
           zerolinecolor = '#ffff',   
           zerolinewidth = 2,   
           gridcolor = 'ffff'),   
         yaxis = list(   
           title='Yoğunluk', 
           zerolinecolor = '#ffff',   
           zerolinewidth = 2,   
           gridcolor = 'ffff'),
         title = 'Recency') 

#Frequency
set.seed(1)    
x1 <- m$Frequency
group_labels = c('Frequency')  
df1 <- data.frame(x1, group_labels[1])  
colnames(df1) <- c('x', 'Group') 
df <- rbind(df1) 
colnames(df) <- c('x', 'Group') 
gg <- ggplot(data = df ) +  
  geom_density(aes(x=x, color=Group)) + geom_rug(aes(x=x, color=Group)) + 
  ylab("") + 
  xlab("")
ggplotly(gg)%>% 
  layout(plot_bgcolor='#e5ecf6',   
         xaxis = list(   
           title='Frequency', 
           zerolinecolor = '#ffff',   
           zerolinewidth = 2,   
           gridcolor = 'ffff'),   
         yaxis = list(   
           title='Yoğunluk', 
           zerolinecolor = '#ffff',   
           zerolinewidth = 2,   
           gridcolor = 'ffff'),
         title = 'Frequency') 
#Geom Point Görselleri

ggplot(data = data) + 
  geom_point(mapping = aes(x = MonetaryValue, y = Frequency ), color = "orange")

ggplot(data = data) + 
  geom_point(mapping = aes(x = MonetaryValue, y = Frequency), color = "orange")+
  geom_smooth(mapping = aes(x = MonetaryValue, y = Frequency), color = "purple")

ggplot(data = data) + 
  geom_point(mapping = aes(x = Recency, y = MonetaryValue  ), color = "pink")

ggplot(data = data) + 
  geom_point(mapping = aes(x = Recency, y = MonetaryValue), color = "Pink")+
  geom_smooth(mapping = aes(x = Recency, y = MonetaryValue), color = "#48dbfb")

#KMeans
library(cluster)
library(tidyverse)
library(factoextra)
library(scales)
library(funModeling)
library(pheatmap)
library(RColorBrewer)
#Korelasyon
library(corrplot)
korelasyon <- cor(m[,c(2,3,4)])
corrplot(korelasyon, method = "circle", bg = "black",title = "korelasyon") #ilişki yoğunluklarını gösterir.
#Birbirinden Farklı K Değerlerine Göre Kümeleme İşlemleri 
k2<- kmeans(m, centers= 2, nstart= 25)
k3<- kmeans(m, centers= 3, nstart= 25)
k4<- kmeans(m, centers= 4, nstart= 25)
k5<- kmeans(m, centers= 5, nstart= 25)

p2 <- fviz_cluster(k2, geom = "point", data = m)+ ggtitle("k = 2")
p3 <- fviz_cluster(k3, geom = "point", data = m)+ ggtitle("k = 3")
p4 <- fviz_cluster(k4, geom = "point", data = m)+ ggtitle("k = 4")
p5 <- fviz_cluster(k5, geom = "point", data = m)+ ggtitle("k = 5")

library(gridExtra)
grid.arrange(p2, p3, p4, p5, nrow = 2) #Rastgele Küme Sayısı

#Optimum küme sayısının bulunmasındaki amaç
#elbow: kümeler içi hata minimum dışındaki hatalar maksimum
library(factoextra)
set.seed(123)
fviz_nbclust(m, kmeans, method = "wss")

#silhoutte: Her bir gözlemin diğer kümeler ile karşılaştırıldığında diğer kümeler ile içinde bulunduğu kümenin ne kadar benzediğini ölçüyor.

set.seed(123)
fviz_nbclust(m, kmeans, method = "silhouette")

#Gap İstatistiği
set.seed(123)
gap_stat <- clusGap (m, FUN= kmeans, nstart= 25, K.max=10, B= 50)
print(gap_stat, method="firstMax")
fviz_gap_stat(gap_stat)

#Sırayla tüm k sayılarını deniyoruz.
set.seed(123)
final <- kmeans(m, 2, nstart=25)
print(final)

set.seed(123)
final <- kmeans(m, 3, nstart=25)
print(final)

set.seed(123)
final <- kmeans(m, 4, nstart=25)
print(final)

set.seed(123)
final <- kmeans(m, 5, nstart=25)
print(final)

set.seed(123)
final <- kmeans(m, 2, nstart=25)
print(final)
classes <- final$cluster
m$sinif <- classes
m
head(m)
tail(m)
library(dplyr) #%>% 
rfm_data %>%
  mutate(Cluster = final$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")
p2 <- fviz_cluster(final, geom = "point", data = m)+ ggtitle("k = 2")
show(p2)
summary(subset(m, sinif == "1"))
summary(subset(m, sinif == "2"))










 




 
