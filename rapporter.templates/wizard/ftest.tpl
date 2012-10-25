<!--head
Title:          F-test
Author:         Rapporter Development Team
Email:          feedback@rapporter.net
Description:    This template will run an F-test to check if two continuous variables have the same variances.
Packages:       nortest
Data required:  TRUE
Example:        rapport('ftest', data=ius2008, xvar='edu', yvar='age')
xvar          | *numeric | X Variable      | Numerical variable
yvar          | *numeric | Y Variable      | Numerical variable
head-->

# Introduction

F test compares the variances of two continuous variables. In other words it shows if their variances were statistically different.

We should be careful, while using the F test, because of the strict normality assumption, where strict means approximately normal ditribution is not enough to satisfy that.

# Normality assumption

The Shapiro-Wilk test, the Lilliefors test, the Anderson-Darling test and the Pearson's Chi-square test help us to decide if the above-mentioned assumption existed.

<%=
if (length(xvar) > 5000) {
    h <- htest(xvar, lillie.test, ad.test, pearson.test)
} else {
    h <- htest(xvar, shapiro.test, lillie.test, ad.test, pearson.test)
}
p <- .05
%>

Normality test results for _<%=xvar.label%>_

<%=
h
%>

So, let's draw some conclusions based on applied normality test:

- according to _Shapiro-Wilk test_, the distribution of _<%= xvar.label %>_ is<%= ifelse(h[1, 3] < p, " not", "") %> normal.
- As the test statistic of the _Lilliefors test_ shows us, we <%= ifelse(j[2, 3]<p, "have to reject", "can accept")%> the normality assumption of _<%= yvar.label %>_.
- _Anderson-Darling test_ confirms<%= ifelse(h[3, 3] < p, " violation of", "") %> normality assumption
- _Pearson's Chi-square test_ classifies the underlying distribution as <%= ifelse(h[4, 3]<p, "non-normal", "normal") %>

<%=k<-0
if (h[1,3]<p){l<-k+1}
if (h[2,3]<p){m<-l+1}
if (h[3,3]<p){n<-m+1}
if (h[4,3]<p){o<-n+1}
%>

In summary we can<%= ifelse(o < 1, "", " not") %> assume, that the distribution of _<%= xvar.label %>_ is statistically normal.

<%=
if (length(yvar) > 5000) {
    j <- htest(yvar, lillie.test, ad.test, pearson.test)
} else {
    j <- htest(yvar, shapiro.test, lillie.test, ad.test, pearson.test)
}
%>

Normality test results for _<%=yvar.label%>_

<%=
j
%>

- The _Shapiro-Wilk test_ shows that _<%= yvar.label %>_ is<%= ifelse(j[1, 3] < p, " not", "") %> normal.
- Based on _Lilliefors test_, distribution of _<%= xvar.label %>_ is <%= ifelse(h[2, 3]<p, "not normal", "normal") %>
- The _Anderson-Darling test_ presents us a<%= ifelse(j[3, 3] < p, " violation of", "") %> normality assumption.
- The distribution of the _<%= yvar.label %>_ is <%= ifelse(j[4, 3]<p, "not normal", "normal") %>, based on the _Pearson's Chi-square test_

<%=q<-0
if (j[1,3]<p){r<-q+1}
if (j[2,3]<p){s<-r+1}
if (j[3,3]<p){t<-s+1}
if (j[4,3]<p){u<-t+1}
%>

As a conclusion of the tests above we can<%= ifelse(u < 1, "", " not") %> assume, that the distribution of _<%= yvar.label %>_ is statistically normal.

## Summary

Thus we should<%= ifelse(o+u < 1, "", " not") %> use the Bartlett's test<%= ifelse(o+u < 1, ".", ", we would rather check the equality of the variances across the groups with the Brown-Forsyth test.") %>

# The F-test

Whereas we have checked the normality assumptions, let's see the results of the _F test_ to compare the variances of <%= p(xvar.label) %> and <%=p(yvar.label)%>.

<%=
ftest <- function(xvar) var.test(xvar,yvar)
Ftest <- htest(xvar,ftest)
f.p <- Ftest$"p-value"
p <- 0.05
Ftest
%>

We can see from the table that there is<%=ifelse(f.p<p,""," not")%> a significant difference between the variance of _<%=xvar.label%>_ and _<%=yvar.label%>_.
