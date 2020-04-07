View(EastWestAirlines) 
library(ggplot2)
library(cluster)
library(dendextend)


#Normalise data to bring data on same scale.
EastWestAirlines<-scale(EastWestAirlines[,2:12])

#Dissimilarity
d<-dist(EastWestAirlines,method ="euclidean")

# Hierarchical clustering using Ward's method
fit<-hclust(d,method="ward.D2")
plot(fit)

#to plot dedogram
fit <- as.dendrogram(fit) 
cd = color_branches(fit,k=3)
plot(cd)

groups <- cutree(fit, k=3) # cut tree into 3 clusters

table(groups)
plot(groups)


output<-as.matrix(groups) # groups or cluster numbers
final <- data.frame(EastWestAirlines, output)
final1 <- final[,c(ncol(final),1:(ncol(final)-1))]
final1

summary(final1)
plot(final1)

#as we have more homogenius members in Cluster 2 and in cluster 3 there are less homogenius members and least is cluster1.
######################################################with complete method##################

#Dissimilarity
d<-dist(EastWestAirlines,method ="euclidean")

# Hierarchical clustering using Ward's method
fit1<-hclust(d,method="complete")

#to plot dedogram
fit1 <- as.dendrogram(fit1) 
cd1 = color_branches(fit1,k=4)
plot(cd1)

groups1 <- cutree(fit, k=4) # cut tree into 3 clusters

table(groups1)
plot(groups1)
output1<-as.matrix(groups1) # groups or cluster numbers
final1 <- data.frame(EastWestAirlines, output1)
finaloutput <- final[,c(ncol(final),1:(ncol(final)-1))]
finaloutput

###more homogenius members is in cluster 2 than cluster 3 than cluster 4 and the least number of members is under cluster 1
