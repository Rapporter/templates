<!--head
Title:          Crosstable
Author:         Rapporter Development Team
Email:          feedback@rapporter.net
Description:    Crosstable of two given variables with count, percentages and tests for indepence
Packages:       descr
Data required:  TRUE
Example:        rapport('crosstable', data=ius2008, row='gender', col='dwell')
		rapport('crosstable', data=ius2008, row='email', col='dwell')
row             | *factor | Row variable        | A categorical variable.
col             | *factor | Column variable     | A categorical variable.
head-->

# Variable description

Two variables specified:

 * "<%=rp.name(row)%>"<%=ifelse(rp.label(row)==rp.name(row), '', sprintf(' ("%s")', rp.label(row)))%> with <%=rp.valid(as.numeric(row))%> valued values and
 * "<%=rp.name(col)%>"<%=ifelse(rp.label(col)==rp.name(col), '', sprintf(' ("%s")', rp.label(col)))%> with <%=rp.valid(as.numeric(col))%> valid values.

TODO: add intr about crosstable

# Counts

<%=
table		<- table(row, col, deparse.level = 0, useNA = 'ifany')
if (length(which(is.na(rownames(table)))) > 0)
    rownames(table)[which(is.na(rownames(table)))] <- 'Missing'
if (length(which(is.na(colnames(table)))) > 0)
    colnames(table)[which(is.na(colnames(table)))] <- 'Missing'
fulltable	<- addmargins(table)
set.caption(sprintf('Counted values: "%s" and "%s"', rp.name(row), rp.name(col)))
set.alignment(row.names = "right")
fulltable
%>

<%=
table.max <- which(table == max(table), arr.ind = TRUE)
%>

Most of the cases (<%=table[table.max]%>) can be found in "<%=paste(rownames(table)[table.max[,1]], colnames(table)[table.max[,2]], sep = '-')%>" categories. Row-wise "<%=names(which.max(rowSums(table)))%>" holds the highest number of cases (<%=max(rowSums(table))%>) while column-wise "<%=names(which.max(colSums(table)))%>" has the utmost cases (<%=max(colSums(table))%>).

# Percentages

<%=
set.caption(sprintf('Total percentages: "%s" and "%s"', rp.name(row), rp.name(col)))
set.alignment(row.names = "right")
fulltable <- round(addmargins(prop.table(table)*100), 2)
fulltable
%>

<%=
set.caption(sprintf('Row percentages: "%s" and "%s"', rp.name(row), rp.name(col)))
set.alignment(row.names = "right")
fulltable <- round(prop.table(addmargins(table, 1), 1)*100, 2)
fulltable
%>

<%=
set.caption(sprintf('Column percentages: "%s" and "%s"', rp.name(row), rp.name(col)))
set.alignment(row.names = "right")
fulltable <- round(prop.table(addmargins(table,2 ), 2)*100, 2)
fulltable
%>

# Tests of Independence

In the below tests for [independece](http://en.wikipedia.org/wiki/Independence_(probability_theory)) we assume that the row and column variables are independent of each other. If this [null hypothesis](http://en.wikipedia.org/wiki/Null_hypothesis) would be rejected by the tests, then we can say that the assumption must have been wrong, so there is a good chance that the variables are associated.

## Chi-squared test

TODO: add intro about chi^2

<%=
table  <- table(row, col, deparse.level = 0) # no need for NAs from here
t      <- suppressWarnings(chisq.test(table))
lambda <- lambda.test(table)
cramer <- sqrt(as.numeric(t$statistic) / (sum(table) * min(dim(table))))
t
%>

<%if (t$p.value < 0.05) { %>

It seems that a real association can be pointed out between *<%=rp.name(row)%>* and *<%=rp.name(col)%>* by the *<%=t$method%>* ($\chi$=<%=as.numeric(t$statistic)%> at the degree of freedom being <%=as.numeric(t$parameter)%>) at the [significance level](http://en.wikipedia.org/wiki/P-value) of <%=t$p.value%>.

The association between the two variables seems to be <%=ifelse(cramer < 0.5, "weak", "strong")%> based on Cramer\'s V (<%=cramer%>).

<% } else { %>

It seems that no real association can be pointed out between *<%=rp.name(row)%>* and *<%=rp.name(col)%>* by the *<%=t$method%>* ($\chi$=<%=as.numeric(t$statistic)%> at the degree of freedom being <%=as.numeric(t$parameter)%>) at the significance level of <%=t$p.value%>.

<% } %>

### Adjusted standardized residuals

The residuals show the contribution to rejeting the null hypothesis at a cell level. An extremely high or low value indicates that the given cell had a major effect on the resulting chi-square, so thus helps understanding the association in the crosstable.

<%=
set.caption(sprintf('Residuals: "%s" and "%s"', rp.name(row), rp.name(col)))
set.alignment(row.names = "right")
table	  <- table(row, col, deparse.level = 0)
table.res <- suppressWarnings(CrossTable(table, asresid = TRUE))$asr
table.res <- round(table.res, 2)
table.res.highlow  <- which(table.res < -2 | table.res > 2, arr.ind = TRUE)
table.res
%>

<%=
if (nrow(table.res.highlow) > 0) {
    sprintf('Based on Pearson\'s resuals the following cells seems interesting (with values higher then `2` or lower then `-2`):\n%s', paste(sapply(1:nrow(table.res.highlow), function(i) sprintf('\n * "%s - %s"', rownames(table)[table.res.highlow[i, 1]], colnames(table)[table.res.highlow[i, 2]])), collapse = ''))
} else {
    sprintf('No interesting (higher then `2` or lower then `-2`) values found based on Pearson\'s residuals.')
}
%>

## Fisher Exact Test

TODO: add intro about Fisher's test

<%=
f <- fisher.test(table)
%>

<% if (t$p.value < 0.05) { %>

The variables seems to be dependent based on Fisher's exact test at the [significance level](http://en.wikipedia.org/wiki/P-value) of <%=f$p.value%>.

<% } else { %>

The variables seems to be independent based on Fisher's exact test.

<% } %>

<% if (t$p.value < 0.05 | f$p.value < 0.05) { %>

# Direction of relationship

## Goodman and Kruskal's lambda

TODO: add intro about lambda

<%if (diff(unlist(lambda, use.names = FALSE)) != 0) { %>

Based on [Goodman and Kruskal's lambda](http://en.wikipedia.org/wiki/Goodman_and_Kruskal's_lambda) it seems that *<%=c(rp.name(col),rp.name(row))[which.max(lambda)]%>* ($\lambda$=<%=pander.return(max(as.numeric(lambda)))%>) has an effect on *<%=c(rp.name(col),rp.name(row))[which.min(lambda)]%>* ($\lambda$=<%=min(as.numeric(lambda))%>) if we assume both variables to be nominal.

<% } else { %>

The computed value for [Goodman and Kruskal's lambda](http://en.wikipedia.org/wiki/Goodman_and_Kruskal's_lambda) is the same for both directions: <%=lambda$row%>. For this end, we do not know the direction of the relationship.

<% }} %>

# Charts

TODO: add details and prettify

<%=
set.caption('Mosaic chart')
mosaicplot(table, shade=T, main=NULL)
%>
