<!--head
Title:          Bartlett's test
Author:         Rapporter Development Team
Email:          feedback@rapporter.net
Description:    This template will run the Bartlett test to check the equality of variances between groups.
Packages:       nortest
Data required:  TRUE
Example:        rapport('bartletts-test', data=ius2008, xvar='edu', yvar='gender')
xvar            | *numeric[1]  | response variable     | Dependent varable
yvar            | *factor[1]   | group variable        | Independent variable
head-->

# Introduction

Bartlett's test is used to test the homogeneity of the variances, in other words the equality of the tested variable's variances across the groups. With checking that we want to find if the two groups are coming from the same population.

Homogeneity is useful to being tested, because that is an assumption of the One-Way ANOVA.

## References

  * Snedecor, George W. and Cochran, William G. (1989). _Statistical Methods_. Iowa State University Press

# Normality assumption

The Bartlett's test has an assumption of normality, thus one should obtain the information if the distribution of the tested variable had a normal distribution.

The Shapiro-Wilk, the Lilliefors, the Anderson-Darling and the Pearson's Chi-square tests help us to do that.

<%=
if (length(xvar) > 5000) {
    h <- htest(xvar, lillie.test, ad.test, pearson.test)
} else {
    h <- htest(xvar, lillie.test, ad.test, pearson.test, shapiro.test)
}
p <- .05
h
%>

So, the conclusions we can draw with the help of test statistics:

<% if (length(xvar) <= 5000 & !is.na(h[4, 3])) { %>
 - according to _Shapiro-Wilk test_, the distribution of _<%= xvar.label %>_ is<%= ifelse(h[4, 3] < p, " not", "") %> normal
<% }
if (!is.na(h[1, 3])) { %>
 - based on _Lilliefors test_, distribution of _<%= xvar.label %>_ is <%= ifelse(h[1, 3]<p, "not normal", "normal") %>
<% }
if (!is.na(h[2, 3])) { %>
 - _Anderson-Darling test_ confirms<%= ifelse(h[2, 3] < p, " violation of", "") %> normality assumption
<% }
if (!is.na(h[3, 3])) { %>
 - _Pearson's Chi-square test_ classifies the underlying distribution as <%= ifelse(h[3, 3]<p, "non-normal", "normal") %>
<% } %>

<%=
o <- sum((h[1,3]<p), (h[2,3]<p), (h[3,3]<p), na.rm = TRUE)
if (length(xvar) > 5000)
    o <- sum(o, (h[4,3]<p), na.rm = TRUE)
%>

As a result we can<%= ifelse(o < 1, "", " not") %> assume, that the distribution of _<%= xvar.label %>_ is statistically normal.

In this case the <%= ifelse(o < 1, " Bartlett's test is advisable to use", "Brown-Forsyth test is more advisable to use.") %>

# Test results

After checking the assumptions let's see what the test shows us!

<%=
bartlett <- function(xvar) bartlett.test(xvar, yvar)
h <- htest(xvar, bartlett)
h
p <- .05
%>

According to the _Bartlett's test_, the variance of the _<%= xvar.label %>_ across the groups of _<%= yvar.label %>_ <%= ifelse(h[1, 3] < p, "significantly differs", "does not differs significantly") %>.

We can conclude that, because <%= ifelse(h[1,3]<p,"the p-value is smaller than 0.05.","the p-value is higher than 0.05.")%>
