<!--head
Title:          K-means cluster
Author:         Rapporter Development Team
Email:          feedback@rapporter.net
Description:    K-means clustering with automatically estimated number of clusters
Packages:       cluster, fpc
Data required:  TRUE
Example:        rapport('cluster-kmeans', data=ius2008, vars=c('age', 'edu', 'leisure'))
		rapport('cluster-kmeans', data=mtcars, vars=names(mtcars))
vars            | *numeric[1,50] | Input variables | Any number of numeric variable
head-->

<%=
## removing NAs and rescaling variables
vars <- na.omit(vars)
varsScaled <- scale(vars)
%>

## Introduction

[K-means Clustering](http://en.wikipedia.org/wiki/K-means_clustering) is a specific and one of the most widespread method of [clustering](http://en.wikipedia.org/wiki/Cluster_analysis). With clustering we want to divide our data into groups, which in the objects are similar to each other. K-means clustering is specified in the way, here we set the number of groups we want to make. In our case we will take into account the following variables: <%=vars.label%>, to find out which observations are the nearest to each other.

## References

J. B. MacQueen (1967). _"Some Methods for classification and Analysis of Multivariate Observations, Proceedings of 5-th Berkeley Symposium on Mathematical Statistics and Probability"_. 1:281-297

## Determining the number of clusters

As it was mentioned above, the speciality of the K-means Cluster method is to set the number of groups we want to produce. Let's see how to decide which is the ideal number of them!

<%=
wss <- (nrow(varsScaled) - 1) * sum(apply(varsScaled, 2, var))
for (i in 2:15) {
    wss[i] <- sum(kmeans(varsScaled, centers = i)$withinss)
}
plot(1:15, wss, type="b", xlab="Number of Clusters",  ylab="Within groups sum of squares")
cn <- pamk(varsScaled)
%>

We can figure out that, as we see how much the Within groups sum of squares decreases if we set a higher number of the groups. So the smaller the difference the smaller the gain we can do with increasing the number of the clusters (thus in this case the larger decreasing means the bigger gain). 
The ideal number of clusters seems to be <%=cn$nc%>.

## Cluster means

The method of the K-means clustering starts with the step to set k number of centorids which could be the center of the groups we want to form. After that there comes several iterations, meanwhile the ideal centers are being calculated. These <%=cn$nc%> centroids seems to be:

<%=
res <- vars[row.names(cn$pamobject$medoids), ]
res$Id <- row.names(res)
row.names(res) <- NULL
if (names(res)[1] != 'Id')
    res <- res[, c(ncol(res), 1:(ncol(vars)))]
set.alignment(c('right', rep('centre', ncol(vars))))
res
%>

The centroids are the observations which are the nearest in average to all the other observations of their group. But it could be also interesting which are the typical values of the clusters! One way to figure out these typical values is to see the group means. The cluster averages are:

<%=
fit <- kmeans(vars, cn$nc)
res <- aggregate(vars, by = list(fit$cluster), FUN = mean)
names(res)[1] <- 'Cluster'
set.alignment(c('right', rep('centre', ncol(vars))))
res
%>

## Results

On the chart below we can see the produced groups. To distinct which observation is related to which cluster each of the objects from the same groups have the same figure and there is a circle which shows the border of the clusters.

<%=
clusplot(vars, fit$cluster, color = TRUE, shade = TRUE, labels = ifelse(nrow(vars) < 100, 2, 4), lines = 1, main = '', col.p = 'black', col.clus = panderOptions('graph.colors'))
%>

