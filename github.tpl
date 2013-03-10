<!--head
Title: GitHub repository statistics
Description: Querying GitHub API for repository statistics and rendering an overview graph
Author: Rapporter Team (@rapporter)
Packages: ggplot2, RCurl, RJSONIO, plyr
Data required: FALSE
user | string[0,500]=Rapporter      | GitHub username | GitHub username
repo | string[0,500]=rapport        | GitHub repository name | GitHub repository name
head-->

<%=
t    <- Sys.time()
url  <- paste('https://github.com/', user, repo, sep = '/')
page <- getURL(url)
raw  <- fromJSON(page)
df   <- as.data.frame(do.call(rbind,lapply(raw, function(x) {
    res <- ldply(x$weeks, data.frame)
    res$author <- x$author[[1]]
    res
})))
df$w <- as.Date(df$w)
%>

# Commits overview

We have fetched <%=nrow(df)*4%> records from [http://github.com](<%=url%>) in <%=as.numeric(Sys.time()-t)%> seconds with the following descriptives for the number of commits a week per user:

<%=
res <- rp.desc('c', 'author', data = df, fn = c('min', 'mean', 'median', 'max', 'sd', 'IQR'))
row.names(res) <- res$author
res$author <- NULL
res
authors <- ddply(df, .(author), summarize, commits = sum(c), additions = sum(a), deletions = sum(d))
weekly <- ddply(df, .(w), summarize, commits = sum(c), additions = sum(a), deletions = sum(d))
%>

Where the overall number of commits for each week was:

<%=
set.caption(paste0('Distribution: number of commits per week', json$name))
hist(weekly$c, main = 'Weekly commits', xlab = '')
%>

But we should definitely check out this data as a time-series :)

<%=
ggplot(df, aes(x=w, y=c)) + geom_area(aes(fill=author,group = author), position='stack') + theme_bw()
%>

For a more detailed overview we would need a more detailed data set, right?

Okay, calling in GitHub API:

% last 3 months

<%
evalsOptions('height', 288)
ids <- which(as.POSIXlt(df$Date)$year %in% years[(1:5) + (i-1)*5])
calendarHeat(df$Date[ids], df$Value[ids] )
%>

# if long -> TS


# A model

And now we build a really simple model based on the year, the day of the month and also the day of the week to predict <%=json$name%>. The model we build is: `value ~ year + mday + wday`

<%=
df$wday <- as.POSIXlt(df$Date)$wday
df$mday <- as.POSIXlt(df$Date)$mday
df$year <- as.POSIXlt(df$Date)$year+1970
fit <- lm(df$Value ~ df$year + df$wday + df$mday)
set.caption('A dummy model')
fit
%>

Most model parameters can be read from the above table, but nothing about the goodness of fit. Well, the R-squared turned out to be <%=summary(fit)$r.squared%> while the adjusted version is <%=summary(fit)$adj.r.squared%>.

Let us also check out the residuals of the above dummy model:

<%=
par(mfrow = c(2, 2))
+plot(fit)
%>
