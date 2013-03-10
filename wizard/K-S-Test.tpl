<!--head
Title:          Kolmogorov-Smirnov-test
Author:         Rapporter Development Team
Email:          feedback@rapporter.net
Description:    This template will run a Kolmogorov-Smirnov-test
Packages:       nortest
Data required:  TRUE
Example:        rapport('K-S-test', data=ius2008, xvar='edu', yvar='age')
xvar          | *numeric | X Variable      | Numerical variable
yvar          | *numeric | Y Variable	   | Numerical variable
head-->

## Introduction

[Kolmogorov-Smirnov test](http://en.wikipedia.org/wiki/Kolmogorov%E2%80%93Smirnov_test) is one of the most widely used [nonparametric tests](http://en.wikipedia.org/wiki/Non-parametric_statistics). With the help of that in this case we use to check if two continuous variables had the same distribution. We do not test that here, but there is a possibility to use that in the way to check if a sample/variable followed an expected distribution.

## Distributions

Before we use the K-S test to look at the possible statistical differences, it could be useful to see visually the distributions we want to observe. Below lie the [histograms](http://en.wikipedia.org/wiki/Histogram) of the variables we compared:

<%=
set.caption(sprintf('Histogram of %s', xvar.label))
hist(xvar)
set.caption(sprintf('Histogram of %s', yvar.label))
hist(yvar)
%>

## Test results

Now we will test if the <%=xvar.label%> and the <%=yvar.label%> had statistically the same distribution.

<%=
set.caption(sprintf('Two-sample Kolmogorov-Smirnov test on %s and %s', xvar.label, yvar.label))
kstest <- suppressWarnings(ks.test(xvar,yvar))
kstest
ksp <- kstest$'p.value'
p <- 0.05
%>

<%if (inherits(tryCatch(ks.test(xvar,yvar), warning = function(w) w), 'warning')) { %>
The requirements of the Kolmogorov-Smirnov Test test was not met, the approximation may be incorrect.
<% } %>
<%=ifelse(ksp>p,'So the nullhypothesis, that the variables follow the same distribution was not rejected. Here the stars represent the [significance levels](http://en.wikipedia.org/wiki/Statistical_significance) of the Kolmogorov-Smirnov test coefficients: one star for `0.05`, two for `0.01` and three  for `0.001`.','So the variables do not follow the same distribution, according to the Kolmogorov-Smirnov test statistic.')%>
