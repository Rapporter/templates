<!--head
Title: Compare Wikipedia search queries
Description: Compare the daily number of Wikipedia searches for various subjects.
Author: Rapporter Team (@rapporter)
Packages: plyr, lubridate, stringr, ggplot2, RJSONIO, forecast
Data required: FALSE
stringsO | SPSS,SAS,STATA,MATLAB | Subject | Choose
stringsF | string[0,500]=cloud computing | Free-text | A comma separated list
lookback | number[1,12]=5 | Months to look back | Months to look back
logplot  | FALSE | Logaritmic scale | Logaritmic scale
head-->

<%=
lookback <- round(lookback)
t        <- Sys.time()
dates    <- format(seq(Sys.time() - months(lookback), Sys.time(), by = "month"), '%Y%m')
datesS   <- format(parse_date_time(dates, 'YM'), '%Y %b')
df       <- data.frame(count = numeric(), date = character(), name = character())
stringsA <- unique(c(stringsO, strsplit(stringsF, ',')[[1]]))

for (s in stringsA) {

    res <- data.frame(count = numeric())

    for (d in dates) {
        res <- rbind(res, as.data.frame(fromJSON(paste('http://stats.grok.se/json/en/', d, s, sep = '/'))$daily_views))
    }

    res$date <-  as.Date(rownames(res))
    res$name <- s
    colnames(res) <- c("count", "date", "name")
    res <- arrange(res, date)
    df  <- rbind(df, res)

}
%>

We have fetched <%=nrow(df)*3%> records from [http://stats.grok.se](http://stats.grok.se) in <%=as.numeric(Sys.time()-t)%> seconds with the following descriptives:

<%=
    rp.desc('count', 'name', data = df, fn = c('min', 'mean', 'median', 'max', 'sd', 'IQR'))
%>

# Overview

<%=
set.caption(p(stringsA))
if (logplot) {
    ggplot(df, aes(x = date, y = log10(count), group = name, colour = name)) + geom_line() + ylab("log10") + xlab("")
} else {
    ggplot(df, aes(x = date, y = count, group = name, colour = name)) + geom_line() + ylab("") + xlab("")
}
%>

# A basic time-series analysis

Here we will try to apply some standard univariate methods to the fetched data.

<%
for (n in stringsA) {
%>

## <%=n%>

<%=
d <- subset(df, name == n)
d <- na.omit(d)
set.caption(paste('Wikipedia searches for', n, 'between ', p(c(head(datesS, 1), tail(datesS, 1)))))
plot(x=d$date, y=d$count, type = "l")
%>

First, let us check the possible auto-correlation values:

<%=
fc  <- panderOptions('graph.fontcolor')
fbs <- panderOptions('graph.fontsize')
bc  <- panderOptions('graph.background')
gc  <- panderOptions('graph.grid.color')
cex <- fbs/12
cs <- panderOptions('graph.colors')
if (panderOptions('graph.color.rnd'))
    cs <- sample(cs)
cb <- cs[1]
par(
    family   = panderOptions('graph.fontfamily'),
    cex      = cex, cex.axis = cex * 0.8, cex.lab = cex, cex.main = cex * 1.2, cex.sub = cex,
    bg       = bc,
    las      = panderOptions('graph.axis.angle'),
    lwd      = 2,
    pch      = panderOptions('graph.symbol'),
    col.axis = fc, col.lab = fc, col.main = fc, col.sub = fc)
+acf(d$count, main = paste('Autocorrelation for', n))
+grid(lty = panderOptions('graph.grid.lty'), col = panderOptions('graph.grid.color'), lwd = 0.5)
%>

And fitting some ARIMA models:

<%=
fit <- tryCatch(auto.arima(ts(d$count, frequency = 365)), error = function(e) e)
%>

<% if (!inherits(fit, 'error')) { %>

<%= fit$var.coef %>

Where AIC equals to <%=fit$aic%>, AICc to <%=fit$aicc%> and BIC to <%=fit$bic%>.

<% } else { %>

Damn, we could not fit a model: `<%=fit$message%>`

<% }} %>
