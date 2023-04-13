library(scatterplot3d)

#data
install.packages("readxl")
library(readxl)
db = read_excel("diabetes.xls")
db = db[,-1]

head(db)
dim(db)

sum(is.na(db)) #결측치 존재x

#거리 표준화
db2 = as.data.frame(scale(db))
head(db2)

##### Hierarchical clustering
h.clust = hclust(dist(db2), method="complete")
plot(h.clust)

#군집 수 결정
install.packages("NbClust")
library(NbClust)
dc = NbClust(db2, distance = 'euclidean', min.nc = 2, max.nc = 15, method = 'complete')

par(mfrow=c(1,1))
barplot(table(dc$Best.n[1,]),xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen")

## Cut the dendrograms
h.cut = cutree(h.clust, k=5)
h.cut
table(h.cut)

## Showing the results
h.seg1 = db2[h.cut==1,]
h.seg2 = db2[h.cut==2,]
h.seg3 = db2[h.cut==3,]
h.seg4 = db2[h.cut==4,]
h.seg5 = db2[h.cut==5,]

pie(table(h.cut),main="number of observations in segment")
h.mean = rbind(apply(h.seg1,2,mean),apply(h.seg2,2,mean),apply(h.seg3,2,mean),apply(h.seg4,2,mean),apply(h.seg5,2,mean))
rownames(h.mean) = c(1,2,3,4,5)
h.mean
dist(h.mean, method = "euclidean", diag = TRUE)



## Boxplot
par(mfrow=c(3,2))
boxplot(h.seg1[,1],h.seg2[,1],h.seg3[,1],h.seg4[,1],h.seg5[,1],ylab=names(h.seg1)[1],xlab="segment",col="blue",names=c(1,2,3,4,5))
boxplot(h.seg1[,2],h.seg2[,2],h.seg3[,2],h.seg4[,2],h.seg5[,2],ylab=names(h.seg1)[2],xlab="segment",col="blue",names=c(1,2,3,4,5))
boxplot(h.seg1[,3],h.seg2[,3],h.seg3[,3],h.seg4[,3],h.seg5[,3],ylab=names(h.seg1)[3],xlab="segment",col="blue",names=c(1,2,3,4,5))
boxplot(h.seg1[,4],h.seg2[,4],h.seg3[,4],h.seg4[,4],h.seg5[,4],ylab=names(h.seg1)[4],xlab="segment",col="blue",names=c(1,2,3,4,5))
boxplot(h.seg1[,5],h.seg2[,5],h.seg3[,5],h.seg4[,5],h.seg5[,5],ylab=names(h.seg1)[5],xlab="segment",col="blue",names=c(1,2,3,4,5))


plot(db2$X4, db2$X5,col=h.cut, pch = h.cut)
plot(db2$X1, db2$X2,col=h.cut, pch = h.cut)





############average#######
h.clust2 = hclust(dist(db2), method="average")
plot(h.clust2)

#군집 수 결정
install.packages("NbClust")
library(NbClust)
dc2 = NbClust(db2, distance = 'euclidean', min.nc = 2, max.nc = 15, method = 'average')

par(mfrow=c(1,1))
barplot(table(dc2$Best.n[1,]),xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen")

## Cut the dendrograms
h.cut2 = cutree(h.clust2, k=4)
h.cut2
table(h.cut2)

## Showing the results
h.seg1 = db2[h.cut2==1,]
h.seg2 = db2[h.cut2==2,]
h.seg3 = db2[h.cut2==3,]
h.seg4 = db2[h.cut2==4,]

pie(table(h.cut2),main="number of observations in segment")
h.mean = rbind(apply(h.seg1,2,mean),apply(h.seg2,2,mean),apply(h.seg3,2,mean), apply(h.seg4,2,mean))
rownames(h.mean) = c(1,2,3,4)
h.mean
dist(h.mean, method = "euclidean", diag = TRUE)



## Boxplot
par(mfrow=c(3,2))
boxplot(h.seg1[,1],h.seg2[,1],h.seg3[,1],h.seg4[,1],ylab=names(h.seg1)[1],xlab="segment",col="blue",names=c(1,2,3,4))
boxplot(h.seg1[,2],h.seg2[,2],h.seg3[,2],h.seg4[,2],ylab=names(h.seg1)[2],xlab="segment",col="blue",names=c(1,2,3,4))
boxplot(h.seg1[,3],h.seg2[,3],h.seg3[,3],h.seg4[,3],ylab=names(h.seg1)[3],xlab="segment",col="blue",names=c(1,2,3,4))
boxplot(h.seg1[,4],h.seg2[,4],h.seg3[,4],h.seg4[,4],ylab=names(h.seg1)[4],xlab="segment",col="blue",names=c(1,2,3,4))
boxplot(h.seg1[,5],h.seg2[,5],h.seg3[,5],h.seg4[,5],ylab=names(h.seg1)[5],xlab="segment",col="blue",names=c(1,2,3,4))
plot(db2$X3, db2$X4,col=h.cut2, pch = h.cut2)
plot(db2$X1, db2$X2,col=h.cut2, pch = h.cut2)






