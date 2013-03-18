<!--head
Title: Analysing time-series of Quandl
Description: Fetching data from Quandle and analysing those with standard time-series statistical methods
Author: Rapporter Team (@rapporter)
Packages: lattice, RJSONIO, grid, chron, makeR, ggplot2, reshape, gvlma, car, forecast
Data required: FALSE
provider | string[0,500]=FRED       | Data provider | Short name found on Quandl site
dataset  | string[0,500]=SP500      | Dataset       | Short name found on Quandl site
from     | string[0,10]=            | Start date    | In the form of 'YYYY-MM-DD'
to       | string[0,10]=            | End date      | In the form of 'YYYY-MM-DD'
variable | string[0,500]=           | Variable      | Variable name in the dataset
head-->

# Metadata

<%=
auth_token <- ''
panderOptions('table.split.table', Inf)
t    <- Sys.time()
url  <- paste0('http://www.quandl.com/api/v1/datasets/', provider, '/', dataset, '.json?sort_order=asc')
if (auth_token != '') {
    url  <- paste0(url, '&auth_token=', auth_token)
}
if ((to.len == 1 & from.len == 1) && (nchar(to) == 10 & nchar(from) == 10)) {
    url <- paste0(url, '&trim_start=', from, '&trim_end=', to)
}
json <- tryCatch(fromJSON(url, nullValue=as.numeric(NA)), error = function(e) e)
%>

<% if (!inherits(json, 'error')) { %>

Analysing *<%=json$name%>* <% if ((to.len == 1 & from.len == 1) && (nchar(to) == 10 & nchar(from) == 10)) { %>between <%=from%> and <%=to%><% } %>downloaded from [Quandl](<%=paste('http://www.quandl.com', provider, dataset, sep = '/')%>) in <%=as.numeric(Sys.time()-t)%> second<%=ifelse(as.numeric(Sys.time()-t)>2,'s', '')%> with the following original description:

> <%=gsub('\n', ' ', json$description)%>

## Variables

<%=
d               <- as.data.frame(matrix(unlist(json$data), ncol = length(json$column_names), byrow = TRUE))
if (ncol(d) > 2) {
    d[, 2:ncol(d)]  <- apply(d[, 2:ncol(d)], 2, as.numeric)
} else {
    d[, 2]  <- as.numeric(d[, 2])
}
names(d)        <- json$column_names
names(d)[1]     <- 'Date'
d$Date          <- as.Date(d$Date)
%>

<%=
if (variable.len == 1 && variable != '') {
    if (variable %in% names(d)) {
        d           <- data.frame(Date = d$Date, d[, variable])
        names(d)[2] <- variable
        dna         <- which(is.na(d[, variable]))
        if (length(dna) > 0) {
            d <- d[-dna, ]
        }
    } else {
        stop('Undefined variable selected. Ignoring `variable` parameter.')

    }
}
if ((variable.len == 0 | variable == '') && ncol(d) == 2) {
    variable <- names(d)[2]
}
%>

This <%=json$frequency%> dataset contains <%=length(json$data)%> row<%=ifelse(length(json$data)>1,'s','')%> and <%=length(json$column_names)%> columns with the overall number of <%=length(json$data) * length(json$column_names)%> records<% if (ncol(d) > 2) { %>. As the dataset is made of several variables to analyse, please choose and click on one from the below list for futher univariate analysis:
<%for (v in tail(json$column_names, -1)) { %>
  * [<%=v%>](<%=paste0('https://rapporter.net/api/rapplicate/?token=', ifelse(exists('token'), token, '#'), '&output_format=html&new_tab=true&provider=', provider, '&dataset=', dataset, '&variable=', v, ifelse((to.len == 1 & from.len == 1) && (nchar(to) == 10 & nchar(from) == 10), paste0('&trim_start=', from, '&trim_end=', to), ''))%>)
<% } %>

# Overview

Until then, let us check a line chart of the dataset:

<%=
dfm                  <- melt(d, id='Date')
levels(dfm$variable) <- sapply(strwrap(levels(dfm$variable), 10, simplify = FALSE), function(x) paste(x, collapse = '\n'))%><%=
panderOptions('graph.legend.position', 'bottom')
evalsOptions('width', 640)%><%=
suppressMessages(suppressWarnings(ggplot(dfm, aes(x = Date, y = value, color = variable)) + geom_line() + ggtitle(json$name) + labs(x = '', y = '') + guides(color=guide_legend(title=NULL)) ))
%>

There can be seen the <%=ncol(d)-1%> variables on the vertical axis based on the date shown on the horizontal axis.

References:

  * Tukey, J. W. (1977).  _Exploratory Data Analysis_, Reading Massachusetts: Addison-Wesley.
  * Sarkar, Deepayan (2008) _Lattice: Multivariate Data Visualization with R_, Springer.

# Pair-wise cross-correlation

We can compare the above visualized time-series for relationships between those by the [cross-correlation function](http://en.wikipedia.org/wiki/Cross-correlation#Time_series_analysis) that is basically a simple [Pearson correlation](http://en.wikipedia.org/wiki/Correlation_and_dependence) estimate when the lag is 0. The negative and positive numbers on the x axis below shows a lag or delay on a <%=json$frequency%> basis, which means that we are looking for a possible temporal effect between the variables.

References:

  * Venables, W. N. and Ripley, B. D. (2002) _Modern Applied Statistics with S_.  Fourth Edition.  Springer-Verlag.
<%for (i in 2:(ncol(d) - 1)) { %>
## <%=p(names(d)[i:(i+1)], sep = ' - ')%>

<%=
dt  <- d[, c(1, i, i+1)]
dfm <- melt(dt, id='Date')
panderOptions('graph.legend.position', 'bottom')
evalsOptions('height', 320)%><%=
suppressMessages(suppressWarnings(ggplot(dfm, aes(x = Date, y = value, color = variable)) + geom_line() + ggtitle(json$name) + labs(x = '', y = '') + guides(color=guide_legend(title=NULL)))) + geom_smooth()
%><%=evalsOptions('height', 480)%>

<%=
l  <- ccf(d[, i], d[, i+1], na.action = na.pass, plot = FALSE)
lm <- which.max(abs(l$acf))
plot(l, main = '')
%>

It seems that the cross-correlation estimate for <%=names(d)[i:(i+1)]%> is maximum at lag <%=l$lag[lm]%> being _<%=format(l$acf[lm], digits = 2)%>_. So it seems that <%=names(d)[i:(i+1)]%> are <%=ifelse(abs(l$acf[lm]) > 0.5, ' ', 'not ')%>correlated.

<% }} else { %>, from which <%=nrow(d)*ncol(d)%> records will be analysed for the _<%=variable%>_ variable.

# Overview

The descriptive statistics of _<%=variable%>_ can be used to provide a quick and dirty overview of the data:

<%=
v   <- d[, variable]
res <- rp.desc(variable, data = d, fn = c('min', 'mean', 'median', 'max', 'sd', 'IQR'))
res$Variable <- NULL
res
%>

## Histogram

The below histogram also shows that the values are somewhere between <%=rp.min(v)%> and <%=rp.max(v)%> (range: <%=diff(range(v, na.rm = TRUE))%>) with the average mean being <%=rp.mean(v)%>:

<%=
set.caption(paste0('Histogram of ', variable))
hist(d[, variable], main = json$name, xlab = '')
%>

If the data is not normal, it is also worth checking out the median (<%=median(v, na.rm = TRUE)%>) and the interquartile range (<%=IQR(v, na.rm = TRUE)%>) too instead of the standard deviation (<%=sd(v, na.rm = TRUE)%>).

References:

  * Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988) _The New S Language_.  Wadsworth & Brooks/Cole.
  * Chambers, J. M. and Hastie, T. J. (1992) _Statistical Models in S_.  Wadsworth & Brooks/Cole.
  * Venables, W. N. and Ripley. B. D. (2002) _Modern Applied Statistics with S_.  Springer.

# Observed values

The above histogram shows not much about a time-series, right? Let us check out other options.

## Line plot

The <%=json$frequency%> data between <%=as.character(d$Date[1])%> and <%=as.character(tail(d$Date, 1))%> on a line-plot:

<%=
f <- switch(json$frequency,
	'daily'    = 365,
	'monthly'  = 12,
	'quaterly' = 4,
	'yearly'   = 1,
	1)
t <- ts(d[, variable], frequency = f, start = c(as.POSIXlt(d$Date[1])$year+1900, as.POSIXlt(d$Date[1])$mon + 1, as.POSIXlt(d$Date[1])$mday))
panderOptions('graph.axis.angle', 0)
evalsOptions('height', 320)%><%=
plot(t, xlab = '', ylab = '', main = json$name)
evalsOptions('height', 480)
%>

## Heatmap

<% if (json$frequency == 'daily') { %>
Which looks much better on a calendar heatmap:

<%
dt  <- d[which(as.POSIXlt(d$Date)$year >= tail(as.POSIXlt(d$Date)$year, 1)-4), ]
yrs <- unique(as.POSIXlt(dt$Date)$year)
evalsOptions('height', max(480 / 5 * length(yrs), 150))
%>

<%=
calendarHeat(dt$Date, dt[, variable])
evalsOptions('height', 480)
%>

<% if (length(unique(as.POSIXlt(d$Date)$year)) > 5) { %>
Please note that only the last 5 years were shown above. Please register at [rapporter.net](http://rapporter.net) for dedicated resources.
<% } %>

References:

  * Paul Bleicher (2009) *calendarHeat* R function
  * Jason Bryer (2012) makeR: Package for managing projects with multiple versions derived from a single source repository. R package version 1.0.2. http://CRAN.R-project.org/package=makeR

<% } else { %>

A calendar heatmap cannot be generated for a(n) <%=json$frequency%> data. Please try again with a daily dataset.

<% } %>

# Autocorrelation

Computing the [cross-correlation](http://en.wikipedia.org/wiki/Autocorrelation) of a signal with itself is a mathematical tool for finding repeating patterns in the time-series. Basically we compute the correlation coefficient between the raw data and its lagged version for serveral iterations, where high (>0.5) or low (<-0.5) values show a repeating pattern.

<% if (any(is.na(d[, variable]))) { %>
Please note, that the original dataset contained missing values, so the estimate computed may well not be a valid autocorrelation.
<% } %>

<%=
l <- tryCatch(acf(d[, variable], na.action = na.omit, plot = FALSE), error = function(e) e)
if (inherits(l, 'error')) {
    l <- acf(d[, variable], na.action = na.pass, plot = FALSE)
}
lm <- which.max(abs(l$acf)[-1]) + 1
set.caption(paste0('The autocorrelation estimate is maximum at lag ', l$lag[lm], ' being ', format(l$acf[lm], digits = 2), '.'))
plot(l, main = '')
%>

References:

  * Venables, W. N. and Ripley, B. D. (2002) _Modern Applied Statistics with S_.  Fourth Edition.  Springer-Verlag.

# Seasonal effects

<% if (json$frequency == 'annual') { %>

There is no sense in checking for seasonality in an annual dataset.

<% } else { if (nrow(d) < f*2) { %>

There are not enough (less than 2 periods of) data in the time series, so seasonal decomposition is not attemplted.

<% } else { %>
Computing a quick and dirty [seasonal-effect](http://en.wikipedia.org/wiki/Decomposition_of_time_series) with the frequency being <%=f%>:

<%=
if (any(is.na(t))) {
    tt <- ts(na.exclude(t), freq = f)
} else {
    tt <- t
}
dct <- decompose(tt)
plot(dct, xlab='')
%>

Where the seasonal effect for a period looks like:

<%=
plot(dct$seasonal[1:f], xlab='', type = 'l', ylab = '')
%>

<% if (any(is.na(t))) { %>Please note that the decomposition might not be valid, as the data contained missing values that were removed. <% } %>

References:

  * M. Kendall and A. Stuart (1983) _The Advanced Theory of Statistics_, Vol.3, Griffin. pp. 410-414.

<% }} %>

# Linear model

<%=
ms <- switch(json$frequency,
            'daily'   = 'value ~ year + month + mday + wday',
            'monthly' = 'value ~ year + month',
            'quaterly'= 'value ~ year + month',
            'annual'  = 'value ~ year',
            'value ~ year + month + mday + wday')
d$wday  <- as.POSIXlt(d$Date)$wday
d$month <- as.POSIXlt(d$Date)$mon
d$mday  <- as.POSIXlt(d$Date)$mday
d$year  <- as.POSIXlt(d$Date)$year + 1900
fit     <- switch(json$frequency,
                  'daily'   = lm(d[, variable] ~ d$year + d$month + d$wday + d$mday),
                  'monthly' = lm(d[, variable] ~ d$year + d$month),
                  'quaterly'= lm(d[, variable] ~ d$year + d$month),
                  'annual'  = lm(d[, variable] ~ d$year),
                  lm(d[, variable] ~ d$year + d$month + d$wday + d$mday))
gvmodel <- tryCatch(gvlma(fit), error = function(e) e)
%>

And now we build a really simple linear model based on <%=switch(json$frequency,
                                                        'daily'   = 'the year, the month, the day of the month and also the day of the week',
                                                        'monthly' = 'the year and the month',
                                                        'quaterly'= 'the year and the month',
                                                        'annual'  = 'the year',
                                                        'the year, the month, the day of the month and also the day of the week')%> to predict <%=json$name%>. The model that can be built automatically is: `<%=ms%>`.

<% if (!inherits(gvmodel, 'error')) { %>
## Assumptions

In order to have reliable results, we have to check if the assumptions of the linear regression met with the data we used:

<%=
(summary(gvmodel))
decision <- (gvmodel$Decision)
decision <- summary(gvmodel)[,3]
decision1 <- (decision[1] == 'Assumptions NOT satisfied!')
decision2 <- (decision[2] == 'Assumptions NOT satisfied!')
decision3 <- (decision[3] == 'Assumptions NOT satisfied!')
decision4 <- (decision[4] == 'Assumptions NOT satisfied!')
decision5 <- (decision[5] == 'Assumptions NOT satisfied!')
decision.any <- (any(decision == 'Assumptions NOT satisfied!'))
%>

To check these assumptions, the [Global Validation of Linear Model Assumptions R-package](http://cran.r-project.org/web/packages/gvlma/index.html) will help us. The result of that we can see in the table above.

The GVLMA makes a thorough detection on the linear model, including tests generally about the fit, the shape of the distribution of the residuals ([skewness](http://en.wikipedia.org/wiki/Skewness) and [kurtosis](http://en.wikipedia.org/wiki/Kurtosis)), the linearity and the [homoskedasticity](http://en.wikipedia.org/wiki/Homoscedasticity). On the table we can see if our model met with the assumptions.
As a generally accepted thumb-rule we use the critical [p-value](http://en.wikipedia.org/wiki/P-value)=0.05.

So let's see the results, which the test gave us:

 - The general statistic tells us about the linear model, that it <%= ifelse(decision1,"does not fit to our data","can fit to our data")%>.

 - According to the GVLMA the residuals of our model's skewness<%= ifelse(decision2,""," does not")%> differs significantly from the normal distribution's skewness.

 - The residuals of our model's kurtosis<%= ifelse(decision3,""," does not")%> differs significantly from the normal distribution's kurtosis, based on the result of the GVLMA.

 - In the row of the link function we can read that the linearity assumption of our model was <%=ifelse(decision4,"rejected","accepted")%>.

 - At last but not least GVLMA confirms<%= ifelse(decision5, " the violation of", "") %> homoscedasticity.

In summary: We can<%=ifelse(decision.any," 't","")%> be sure that the linear model used here fits to the data.

References:

  * Pena, EA and Slate, EH (2006): Global validation of linear model assumptions. _J. Amer. Statist. Assoc._ **101** (473):341-354.

<% } else { %>

We could not test the assumptions as the following R error occured: `<%=gvmodel$message%>`

<% } %>

## Linearity

As we want to fit a linear regression model, it is advisable to see if the relationship between the used variables are linear indeed. Next to the test statistic of the GVLMA it is advisable to use a graphical device as well to check that linearity. Here we will use the so-called crPlots funtion to do that, which is an abbreviation of the Component and Residual Plot.

<%=
crPlots(fit)
%>

First, we can see two lines and several circles. The red interrupted line is the best fitted linear line, which means that te square of the residuals are the least while fitting that line in the model. The green curved line is the best fitted line, which does not have to be straight, of all. The observations we investigate are the circles. We can talk about linearity if the green line did not lie too far from the red.

References:

  * Cook, R. D. and Weisberg, S. (1999) _Applied Regression, Including Computing and Graphics._ Wiley.
  * Fox, J. (2008) _Applied Regression Analysis and Generalized Linear Models_, Second Edition. Sage.
  * Fox, J. and Weisberg, S. (2011) _An R Companion to Applied Regression_, Second Edition, Sage.

## Parameters

<%=
set.caption(paste('A linear model:', ms))
fit
%>

Most model parameters can be read from the above table, but nothing about the goodness of fit. Well, the [R-squared](http://en.wikipedia.org/wiki/Coefficient_of_determination) turned out to be <%=summary(fit)$r.squared%> while the adjusted version is <%=summary(fit)$adj.r.squared%>.

References:

  * Chambers, J. M. (1992) _Linear models._ Chapter 4 of _Statistical Models in S_ eds J. M. Chambers and T. J. Hastie, Wadsworth & Brooks/Cole.

  * Wilkinson, G. N. and Rogers, C. E. (1973) Symbolic descriptions of factorial models for analysis of variance.  _Applied Statistics_, **22**, 392-9.

## Residuals

Let us also check out the [residuals](http://en.wikipedia.org/wiki/Errors_and_residuals_in_statistics) of the above linear model:

<%=
par(mfrow = c(2, 2))
+plot(fit)
%>

References:

  * Belsley, D. A., Kuh, E. and Welsch, R. E. (1980) _Regression Diagnostics._ New York: Wiley.
  * Cook, R. D. and Weisberg, S. (1982) _Residuals and Influence in Regression._ London: Chapman and Hall.
  * Firth, D. (1991) Generalized Linear Models.  In Hinkley, D. V. and Reid, N. and Snell, E. J., eds: Pp. 55-82 in Statistical Theory and Modelling. In Honour of Sir David Cox, FRS.  London: Chapman and Hall.
  * Hinkley, D. V. (1975) On power transformations to symmetry. _Biometrika_ *62*, 101-111.
  * McCullagh, P. and Nelder, J. A. (1989) _Generalized Linear Models._ London: Chapman and Hall.

## Predicted values

At last, let us compare the original data with the predicted values:

<%=
d$wday <- d$mday <- d$month <- d$year <- NULL
d$Predicted <- NA
d$Predicted[which(!is.na(d[, variable]))] <- as.numeric(predict(fit))
dfm         <- melt(d, id='Date')
panderOptions('graph.legend.position', 'bottom')
evalsOptions('height', 320)%><%=
suppressMessages(suppressWarnings(ggplot(dfm, aes(x = Date, y = value, color = variable)) + geom_line() + ggtitle(json$name) + labs(x = '', y = '') + guides(color=guide_legend(title=NULL))))
%>

References:

  * Chambers, J. M. and Hastie, T. J. (1992) _Statistical Models in S_.  Wadsworth & Brooks/Cole.

# ARIMA

Here we try to identify the best [ARIMA model](http://en.wikipedia.org/wiki/Autoregressive_integrated_moving_average) to better understand the data or to predict future points in the series. The model is chosen according to either [AIC](http://en.wikipedia.org/wiki/Akaike_information_criterion), AICc or [BIC](http://en.wikipedia.org/wiki/Bayesian_information_criterion) value is:

<%=
if (length(t) < 365) {
    suppressWarnings(fit <- tryCatch(auto.arima(t), error = function(e) e))

} else {
    fit <- structure(list(message = 'We are terribly sorry, but this computational intensive process\nis not allowed to be run on a time-series with more then 365 values.\nPlease sign up for an account at rapporter.net for extra resources\nor filter your data by date.'), class = 'error')
}
%>

<% if (!inherits(fit, 'error')) { %>

```<%= paste(capture.output(fit), collapse = '\n') %>```

Where AIC equals to <%=fit$aic%>, AICc to <%=fit$aicc%> and BIC to <%=fit$bic%>.

<% } else { %>

Damn, we could not fit a model:

```
<%=fit$message%>
```

<% } %>

References:

  * Hyndman, R.J. and Khandakar, Y. (2008) "Automatic time series forecasting: The forecast package for R", _Journal of Statistical Software_, *26*(3).

<% } %>

<% } else { %>
We are terribly sorry, but could not download data from Quandl: [<%=url <- paste('http://www.quandl.com', provider, dataset, sep = '/');url%>](<%=url%>)
<% } %>
