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

## Determining the number of clusters

<%=
wss <- (nrow(varsScaled) - 1) * sum(apply(varsScaled, 2, var))
for (i in 2:15) {
    wss[i] <- sum(kmeans(varsScaled, centers = i)$withinss)
}
plot(1:15, wss, type="b", xlab="Number of Clusters",  ylab="Within groups sum of squares")
cn <- pamk(varsScaled)
%>

The ideal number of clusters seems to be <%=cn$nc%>.

## Cluster means

The <%=cn$nc%> centroids seems to be:

<%=
res <- vars[row.names(cn$pamobject$medoids), ]
res$Id <- row.names(res)
row.names(res) <- NULL
if (names(res)[1] != 'Id')
    res <- res[, c(ncol(res), 1:(ncol(vars)))]
set.alignment(c('right', rep('centre', ncol(vars))))
res
%>

The cluster averages are:

<%=
fit <- kmeans(vars, cn$nc)
res <- aggregate(vars, by = list(fit$cluster), FUN = mean)
names(res)[1] <- 'Cluster'
set.alignment(c('right', rep('centre', ncol(vars))))
res
%>

## Results

<%=
clusplot(vars, fit$cluster, color = TRUE, shade = TRUE, labels = ifelse(nrow(vars) < 100, 2, 4), lines = 1, main = '', col.p = 'black', col.clus = panderOptions('graph.colors'))
%>

