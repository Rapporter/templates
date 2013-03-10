<!--head
Title:          Homogeneity test of factor variables
Author:         Rapporter Development Team
Email:          feedback@rapporter.net
Description:    Test of homogeneity of a given factor variable split by another factor.
Packages:       descr
Data required:  TRUE
Example:        rapport('homogeneity-crosstable', data=ius2008, var = 'gender', split = 'dwell')
		rapport('homogeneity-crosstable', data=ius2008, var = 'email', split = 'gender')
var             | *factor | Variable to analyse   | A categorical variable.
split           | *factor | Split variable        | A categorical variable.
head-->

# Variable description

Analysing "<%=rp.name(var)%>"<%=ifelse(rp.label(var)==rp.name(var), '', sprintf(' ("%s")', rp.label(var)))%> with <%=rp.valid(as.numeric(var))%> valid values whether frequency counts are distributed equally across different categories of "<%=rp.name(split)%>"<%=ifelse(rp.label(split)==rp.name(split), '', sprintf(' ("%s")', rp.label(split)))%>.

"<%=rp.name(split)%>" has <%=split.cat <- names(table(split)); length(split.cat)%> categories:
<%=as.list(split.cat)%>

# Counts

<%=
table		<- table(split, var, deparse.level = 0, useNA = 'ifany')
if (length(which(is.na(rownames(table)))) > 0)
    rownames(table)[which(is.na(rownames(table)))] <- 'Missing'
if (length(which(is.na(colnames(table)))) > 0)
    colnames(table)[which(is.na(colnames(table)))] <- 'Missing'
fulltable	<- addmargins(table)
set.caption(sprintf('Counted values: "%s" and "%s"', rp.name(split), rp.name(var)))
set.alignment(row.names = "right")
fulltable
%>

# Chi-squared test

Our [null hypothetis](http://en.wikipedia.org/wiki/Null_hypothesis) says that the proportion of *<%=rp.name(var)%>* is indentical in each categories of *<%=rp.name(split)%>*.

<%=
t <- suppressWarnings(chisq.test(table))
t
%>

The chi-squared test returned the value of <%=as.numeric(t$statistic)%> with a degree of freedom being <%=as.numeric(t$parameter)%>. Based on the returned [p value](http://en.wikipedia.org/wiki/P-value) (<%=t$p.value%>) we could state that the null hypothesis is <%=ifelse(t$p.value < 0.05, 'rejected', 'accepted')%>.

