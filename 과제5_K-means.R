install.packages("readxl")     
install.packages("scatterplot3d") #for plotting
install.packages("rgl")           #for plotting
install.packages("dummies")       #for creating dummay variables

library(readxl)
diabetes = read_excel("diabetes.xls")
head(diabetes)
dim(diabetes)

n = nrow(diabetes)
diabetes

diabetes = as.data.frame(scale(diabetes))
diabetes$ID <- NULL
head(diabetes)

attach(diabetes)

## 3D plots

library(scatterplot3d)
scatterplot3d(X5,X2,X3)



#### K-means clustering

k.clust = kmeans(diabetes[,1:5], centers=3, nstart=20)
k.clust$cluster
k.clust$tot.withinss

k.clust2 = kmeans(diabetes[,1:5], centers=3)
k.clust2$tot.withinss


## Showing the results

pie(k.clust$size, main="number of observations in segment")
k.clust$centers
dist(k.clust$centers, method = "euclidean", diag = TRUE)

dev.off()

scatterplot3d(X1, X2, X4, color=k.clust$cluster)

diabetes.pca = princomp(diabetes,cor=T)
plot(diabetes.pca$scores[,1], diabetes.pca$scores[,2],xlab="PC1",ylab="PC2",type="n",lwd=2)
text(diabetes.pca$scores[,1], diabetes.pca$scores[,2], labels=k.clust$cluster,col='blue',cex=1,lwd=1)

## Boxplot

seg1 = diabetes[k.clust$cluster==1,]
seg2 = diabetes[k.clust$cluster==2,]
seg3 = diabetes[k.clust$cluster==3,]

par(mfrow=c(5,1))
boxplot(seg1[,1],seg2[,1],seg3[,1],ylab=names(seg1)[1],xlab="segment",col="blue",names=c(1,2,3))
boxplot(seg1[,2],seg2[,2],seg3[,2],ylab=names(seg1)[2],xlab="segment",col="blue",names=c(1,2,3))
boxplot(seg1[,3],seg2[,3],seg3[,3],ylab=names(seg1)[3],xlab="segment",col="blue",names=c(1,2,3))
boxplot(seg1[,4],seg2[,4],seg3[,4],ylab=names(seg1)[4],xlab="segment",col="blue",names=c(1,2,3))
boxplot(seg1[,5],seg2[,5],seg3[,5],ylab=names(seg1)[5],xlab="segment",col="blue",names=c(1,2,3))


dev.off()





#END

install.packages("factoextra")
library(factoextra)
fviz_nbclust(diabetes, kmeans, method = "wss")



install.packages("NbClust")
library(NbClust)

NbClust(diabetes, min.nc = 2, max.nc=15,method="kmeans", index="all")
par(mfrow=c(1,1))
barplot(table(nc$Best.n[1,]),xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen")
