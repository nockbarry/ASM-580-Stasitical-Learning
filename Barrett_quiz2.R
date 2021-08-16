#AMS 580 Quiz 2 Nicholas Barrett
library("cluster")
#library("factoextra")
data = iris
pc = princomp(data[,1:4], cor = T)
screeplot(pc, type = "lines")

k.means.fit <- kmeans(data[,1:4], 3) 
library(cluster)
clusplot(data, k.means.fit$cluster, main='2D representation of the Cluster solution',color=TRUE, shade=TRUE,labels=2, lines=0)
table(data[,5],k.means.fit$cluster)

#Wald
d <- dist(data[,1:4], method = "euclidean")
H.fit <- hclust(d, method="ward.D")
plot(H.fit)
groups <- cutree(H.fit, k=3)
rect.hclust(H.fit, k=3, border="red")
table(data[,5],groups)
clusplot(data, groups, main='2D representation of the Cluster solution',color=TRUE, shade=TRUE,labels=2, lines=0)
#136/150

#H.Single
H.fit <- hclust(d, method="single")
plot(H.fit)
groups <- cutree(H.fit, k=3)
rect.hclust(H.fit, k=3, border="red")
table(data[,5],groups)
clusplot(data, groups, main='2D representation of the Cluster solution',color=TRUE, shade=TRUE,labels=2, lines=0)
#102/150


#H.Complete
H.fit <- hclust(d, method="complete")
plot(H.fit)
groups <- cutree(H.fit, k=3)
rect.hclust(H.fit, k=3, border="red")
table(data[,5],groups)
clusplot(data, groups, main='2D representation of the Cluster solution',color=TRUE, shade=TRUE,labels=2, lines=0)
#74/150


#H.Average
H.fit <- hclust(d, method="average")
plot(H.fit)
groups <- cutree(H.fit, k=3)
rect.hclust(H.fit, k=3, border="red")
table(data[,5],groups)
clusplot(data, groups, main='2D representation of the Cluster solution',color=TRUE, shade=TRUE,labels=2, lines=0)
#136/150

#H.centroid
H.fit <- hclust(d, method="centroid")
plot(H.fit)
groups <- cutree(H.fit, k=3)
rect.hclust(H.fit, k=3, border="red")
table(data[,5],groups)
clusplot(data, groups, main='2D representation of the Cluster solution',color=TRUE, shade=TRUE,labels=2, lines=0)
#102/150

#Wald or H average would work best for this data, both achievinve 50/50 for the first 2 groups
# and putting to many species in group 2 from group 3
# this method is only viable for determining group 1 and so is not recommended

#Q2
#install.packages("devtools")
#library(devtools)
#install_github("vqv/ggbiplot")
#library(ggbiplot)
ir.pca = prcomp(iris[,1:4], center = TRUE, scale. = TRUE)
summary(ir.pca)
#ggbiplot(ir.pca)
#ggbiplot(ir.pca,ellipse=TRUE,circle=TRUE, labels=rownames(iris), groups=iris$Species)
ir.pca$rotation[,1] #PC1
#Yes, the variance is easily soaked by few principal components
