<!--head
meta:
  title: NBA players in 2012-13
  description: MDS and k-means clustering of player sport-statistics in chosen NBA team.
  author: Rapporter Team (@rapporter)
  packages:
  - cluster
  - directlabels
  - fpc
  - ggplot2
inputs:
- required: yes
  class: character
  name: teamname
  label: Team Name
  standalone: yes
  value: Miami Heat
  length:
    min: 1.0
    max: 1.0
  description: Name of the Team
  matchable: yes
  options:
  - Atlanta Hawks
  - Boston Celtics
  - Brooklyn Nets
  - Charlotte Hornets
  - Chicago Bulls
  - Cleveland Cavaliers
  - Dallas Mavericks
  - Denver Nuggets
  - Detroit Pistons
  - Golden State Warriors
  - Houston Rockets
  - Indiana Pacers
  - Los Angeles Clippers
  - Los Angeles Lakers
  - Memphis Grizzles
  - Miami Heat
  - Milwaukee Bucks
  - Minnesota Timberwolves
  - New Orleans Hornets
  - New York Knicks
  - Oklahoma City Thunder
  - Orlando Magic
  - Philadelphia 76ers
  - Phoenix Suns
  - Portland TrailBlazers
  - Sacramento Kings
  - San Antonio Spurs
  - Toronto Raptors
  - Utah Jazz
  - Washington Wizards
  allow_multiple: no
- required: yes
  class: character
  name: variables
  label: Variables
  standalone: yes
  value: ~
  length:
    min: 2.0
    max: 40.0
  description: Calculations are based on these variables
  matchable: yes
  options:
  - Age
  - Games Played
  - Games Started
  - Minutes Played
  - Field Goals
  - Field Goal Attempts
  - Three-Point Field Goals
  - Three-Point Field Goal Attempts
  - Free Throws
  - Free Throw Attempts
  - Offensive Rebounds
  - Defensive Rebounds
  - Total Rebounds
  - Assists
  - Steals
  - Blocks
  - Turnovers
  - Personal Faults
  - Points
  - Field Goal Percentage
  - Three-Point Field Goal Percentage
  - Free Throw Percentage
  allow_multiple: yes
- required: no
  class: integer
  name: clust_num
  label: Number of Clusters
  standalone: yes
  value: ~
  length:
    min: 1.0
    max: 1.0
  description: One can set the Number of the Clusters for the K-Mean Clustering
  limit:
    min: 1.0
    max: 5.0
head-->

Below you are able to check how the <%=teamname%> build up. Later you can answer the question: are there any statistical base of grouping the players in this team?
It can be interesting to see if there were cliques for the best players, mostly the players differ according to their positions or there are other effects which could be detected.
Rapporter will present you two ways to observe that:

  - [Multidimensional-scaling (MDS)](http://en.wikipedia.org/wiki/Multidimensional_scaling)
  - [K-means Clustering](http://en.wikipedia.org/wiki/K-means_clustering)

## MDS

In this part one can check that how <%=teamname%> looks in a two-dimensional view, so which players are similar to each other and which aren't.
The distances were computed based on the standardized form of the variables: <%= variables%>

<%=
nba <- rp.data
player_name <- nba[nba$Team == teamname,1]
Position <- nba[nba$Team == teamname,2]
nba <- nba[nba$Team == teamname, variables]
rownames(nba) <- player_name
nba <- scale(na.omit(nba))
d <- dist(nba)
fit <- cmdscale(d, eig=TRUE, k = 2)
x <- fit$points[, 1]
y <- fit$points[, 2]
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", xlim=c((min(x)*1.15),(max(x)*1.15)),ylim=c((min(y)*1.15),(max(y)*1.15)), main="Metric  MDS",   type="n")
+text(x, y, labels = row.names(nba), cex=1.1, col=panderOptions("graph.colors")[as.numeric(Position)])
%>

You can see the players colored by their positions:

* <span  style="color:<%=panderOptions("graph.colors")[3] %> ;"> Point Guard </span>
* <span  style="color:<%=panderOptions("graph.colors")[5] %> ;"> Shooting Guard </span>
* <span  style="color:<%=panderOptions("graph.colors")[4] %> ;"> Small Forward </span>
* <span  style="color:<%=panderOptions("graph.colors")[2] %> ;"> Power Forward </span>
* <span  style="color:<%=panderOptions("graph.colors")[1] %> ;"> Center </span>

<%=
distance <- as.matrix(d)
maxind <- which(distance == max(distance), arr.ind = TRUE)
minind <- which(distance == min(distance[distance!=min(distance)]), arr.ind = TRUE)
%>

<%=
furthest <- colnames(distance)[which(colSums(distance) == max(colSums(distance)))]
nearest <- colnames(distance)[which(colSums(distance) == min(colSums(distance)))]%>

<%=furthest %> differ<%= ifelse(length(furthest) > 1, "", "s")%> the most from the others, and <%=nearest%> seem<%= (ifelse (length(nearest) > 1, "", "s"))%> to be the most "common" observation<%= (ifelse (length(nearest) > 1, "s", ""))%>, which <%=ifelse(length(nearest) > 1, "lies", "lie") %> nearest to all other observations.

<%=
distance[upper.tri(distance, diag = T)] <- NA
h <- NULL
notneeded <- apply(data.frame(unique(as.vector(sort(distance[lower.tri(distance)], decreasing = T))[1:5])), 1, function(i) h <<- rbind(h, which(distance == i, arr.ind = T)))
j <- NULL
notneeded <- apply(data.frame(unique(as.vector(sort(distance[lower.tri(distance)], decreasing = F))[1:5])), 1, function(i) j <<- rbind(j, which(distance == i, arr.ind = T)))
%>


#### Outsider Pairs

<%=paste0(p(c(rownames(distance)[h[1, 1]], colnames(distance)[h[1, 2]])), ' (', round(distance[h[1, 1], h[1, 2]], 2), ')')%> are the "furthest", <%=paste0(p(c(rownames(distance)[j[1, 1]], colnames(distance)[j[1, 2]])), ' (', round(distance[j[1, 1], j[1, 2]], 2), ')') %> are the "nearest" to each other.

#### In General

Now let's see which observations can be said statistically far/similar to each other in general. The 5 pairs with the biggest differences and the 5 pairs with the smallest differences will be presented. In the brackets you can see the amount of the distances between two observations.


According to the used variables the 5 furthest pair of observations are:

<%=
paste(pander.return(lapply(1:5, function(i) paste0(p(c(rownames(distance)[h[i, 1]], colnames(distance)[h[i, 2]])), ' (', round(distance[h[i, 1], h[i, 2]], 2), ')'))), collapse = '\n')%>


According to the used variables the 5 nearest pair of observations are:

<%=
paste(pander.return(lapply(1:5, function(i) paste0(p(c(rownames(distance)[j[i, 1]], colnames(distance)[j[i, 2]])), ' (', round(distance[j[i, 1], j[i, 2]], 2), ')'))), collapse = '\n') %>


## K-Means Clustering

Step onto an other field, let's see how a data-mining method would group these players. The K-Means Clustering process gives us an estimation which players form a group.
<%=
if (exists('clust_num') && !is.null(clust_num) && clust_num > 0) {
  cn <- tryCatch(pam(nba, clust_num), error = function(e) e)
  fit <- kmeans(scale(nba), clust_num)
} else {
  cn <- tryCatch(pamk(nba), error = function(e) e)
  if (cn$nc > 5) {
    clust_num <- 5
    cn <- tryCatch(pam(nba, clust_num), error = function(e) e)
    fit <- kmeans(scale(nba), clust_num)
  } else {
    fit <- kmeans(scale(nba), cn$nc)
    cn <- cn$pamobject
  }
}
%>

The size of these clusters are: <%= fit$size %> .

In the first group we can find <%=rownames(nba)[fit$cluster == 1]%>.
<%
if (max(fit$cluster) > 1) { %>
In the second there are: <%=rownames(nba)[fit$cluster == 2]%>.
<% } %>
<%
if (max(fit$cluster) > 2) { %>
The third goup contains: <%=rownames(nba)[fit$cluster == 3]%>.
<% } %>
<%
if (max(fit$cluster) > 3) { %>
The following players are in the fourth group: <%=rownames(nba)[fit$cluster == 4]%>.
<% } %>
<%
if (max(fit$cluster) > 4) { %>
The players who are in the fifth group: <%=rownames(nba)[fit$cluster == 5]
%>.
<% }
%>

Check these groups on a map (the purple lines join the centers of the groups):

<%=
clusplot(cn, fit$cluster, color = TRUE, shade = TRUE, labels = 2, lines = 1, main = '', col.p = 'black', col.clus = panderOptions('graph.colors'))
%>

Are the position and grouping of the players fit your prior expectations? :)
