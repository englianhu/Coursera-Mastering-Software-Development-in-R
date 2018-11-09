The `farsdata` R Package
========

<img src="vignettes/figure/coursera.jpg" width="120" />

This is an assignment for the **Course 3 : Building R Packages**. Kindly refer to my GitHub repo for the courses [Coursera Mastering Software Development in R](https://github.com/englianhu/Coursera-Mastering-Software-Development-in-R) to know the course detail or [Building R Packages](https://www.coursera.org/learn/r-packages) to register for study.

Installation
------------

You can install the released version of farsdata from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages('farsdata')

# Or below code if above code unable install the package
devtools::install_github('englianhu/farsdata')
```

Example
-------

You can load the package and read the sample dataset.

``` r
library(farsdata)
library(maps)

fars_2013_fn <- make_filename(2013)
fars_2013 <- fars_read(fars_2013_fn) 
dim(fars_2013)
## [1] 30202    50
```

The data in this package come from the National Highway Traffic Safety Administration (NHTSA) Fatality Analysis Reporting System (FARS) data.

``` r
## [[1]]
## # A tibble: 30,202 x 2
##    MONTH  year
##    <int> <dbl>
##  1     1  2013
##  2     1  2013
##  3     1  2013
##  4     1  2013
##  5     1  2013
##  6     1  2013
##  7     1  2013
##  8     1  2013
##  9     1  2013
## 10     1  2013
## # ... with 30,192 more rows
```

Vignettes
-------

You can refer to below articles for more infoamtion about the package.

- [Introduction to `farsdata` Package](http://rpubs.com/englianhu/farsdata-intro)
- [Fatality Analysis Reporting System (FARS)](http://rpubs.com/englianhu/farsdata-vignette)
