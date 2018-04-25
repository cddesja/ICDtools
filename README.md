# ICDtools
R Tools for the Institute of Child Development

To install the in-development package:

```r
install.packages("devtools")
devtools::install_github("cddesja/ICDtools")
```

Presently, the package does the following:
- Scores the downward extension of the Flanker task and Dimensional Change Card Sort (DCCS), see `dextScore()`.
- Combines multiply-imputed data sets for `lavaan` models using Rubin's rules, see `rrLavaan()`. EXPERIMENTAL! 
- Creates univariate probability plots from logistic regression models, see `probPlot()`.
- Creates regions of significance plot for understanding moderation, see `rsPlot()`.

Please use the following citation if you use these functions:

Desjardins, C. D. (2018). ICDtools: R tools for the Institute of Child Development. R
  package version 0.1.
