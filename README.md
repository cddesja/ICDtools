# ICDtools
R Tools for the Institute of Child Development

To install the in-development package:

```r
install.packages("devtools")
devtools::install_github("cddesja/ICDtools")
```

Presently the package does the following:
- Scores the DEXT
- Combines multiply-imputed data sets for `lavaan` models using Rubin's rules. EXPERIMENTAL!
- Creates univariate probability plots from logistic regression models.
- Creates regions of significance plot for understanding moderation.
