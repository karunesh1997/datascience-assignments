crime_data <- read.csv("C:/Users/Ratnesh/Downloads/crime_data.csv")
View(crime_data)

#normalize the data
mydata<-scale(crime_data[,2:5])

summary(mydata)
#so we can see that our data in normalized

# see the structure of the data
str(mydata)

# find the distance matrix
d<-dist(mydata,method = "euclidean")
# this is the distance matrix

#building the clustering algorithm
clust<-hclust(d,method = "centroid")

#plot the algorithm
plot(clust)

#cut the dendogram in 4 clusters
group<-cutree(clust,k=4)

#plot the dendogram cut in clusters 
rect.hclust(clust,k=4,border = "red")
# so we can see here 4 clusters

# attach the data points with the clusters
clusters= data.frame('X'=crime_data[,1],'cluster'= group)
