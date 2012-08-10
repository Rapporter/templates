Here goes the development versions of *rapporter* templates.

Assigned developpers are show after each template between parenthesis. If no one is assigned, feel free to take any of them :)

## Traditional statistics templates

Can be found in [rapport package](https://github.com/Rapporter/rapport) (@aL3xa & @daroczig).

## Wizard

The below templates are made for the laymen: those who do not really know any statistical background, just have some ideas what they want to do with their data. Like: is there any difference in the salary between girls and boys?

### Descriptives

This is to be forked from rapport package.

### Bivariate methods

The method would be automatically chosen based on the level of measurement of the provided variables and a short question to the user about what he really want to do. E.g. only association or effects etc.

#### Factor-factor

  * chi-squared homogeneity test (@daroczig)
  * crosstable (@daroczig)

#### Factor-numeric

  * ANOVA OR t-test/Welch (based on the number of categories) with automatic normality test (@tothg & @aL3xa)
  * Levine OR Brown-Forsythe test (automatic test for the underlying distribution) (@tothg)

#### Numeric-numeric

  * (linear) regression
  * correlation (@daroczig)
  * PCA
  * Kolmogorov-Smirnov test
  * CA
  * two sample t-test (@tothg & @aL3xa)
  * F test (@nagydaniel)