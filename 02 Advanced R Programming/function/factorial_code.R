## Factorial_loop
## https://www.brodieg.com/2014/04/18/datatable-vs-dplyr-in-split-apply-comgine/
## https://www.brodieg.com/2014/04/03/performance-impact-of-s3-and-s4-dispatch/
Factorial_loop1 <- function (x) {
  result <- 1
  if (x == 0) return(1)
  for (i in 1:x) {
    result <- result * i
  }
  return(result)
}

Factorial_loop2 <- function(x) {
  if (x == 0 || x == 1)
    return(1)
  for (i in (x - 1):1) {
    x <- x * i
  }
  x
}


### Factorial Reduce
Factorial_reduce1 <- function (x){
  if (x == 0) return(1)
  result <- reduce(as.numeric(1:x), `*`)
  return(result)
}

Factorial_reduce2 <- function(x) {
  if (x == 0)
    return(1)
  reduce(as.numeric(1:x), `*`)
}


### Factorial Func
Factorial_func1 <- function (x) {
  if (x == 0) return(1)
  result <- x * Factorial_func1(x-1)
  return(result)
}

Factorial_func2 <- function(x) {
  if (x == 0)
    return(1)
  x * Factorial_func2(x - 1)
}


## Factorial Memoization
Factorial_mem1 <- function(x) {
  if (x == 0) {
    return(1)
  } 
  fact_tbl[x] <<- x * Factorial_mem1(x-1)
  return(fact_tbl[x])
}

Factorial_mem2 <- function(x) {
  if (x == 0)
    return(1)
  if (!is.na(fact_tbl)[x])
    return(fact_tbl[x])
  fact_tbl[x] <<- x * Factorial_mem2(x - 1)
  fact_tbl[x]
}

fact_tbl <- c(rep(NA, 100))


## Comparison
smp <- 100
factorial(100)
## [1] 9.332622e+157

smp %>% 
  l_ply(function(x) {
    cat('Factorial_loop1   is', x %>% Factorial_loop1, '\n')
    cat('Factorial_reduce1 is', x %>% Factorial_reduce1, '\n')
    cat('Factorial_func1   is', x %>% Factorial_func1, '\n')
    cat('Factorial_mem1    is', x %>% Factorial_mem1, '\n')
    cat('Factorial_loop2   is', x %>% Factorial_loop2, '\n')
    cat('Factorial_reduce2 is', x %>% Factorial_reduce2, '\n')
    cat('Factorial_func2   is', x %>% Factorial_func2, '\n')
    cat('Factorial_mem2    is', x %>% Factorial_mem2, '\n')
  })
## Factorial_loop1   is 9.332622e+157 
## Factorial_reduce1 is 9.332622e+157 
## Factorial_func1   is 9.332622e+157 
## Factorial_mem1    is 9.332622e+157 
## Factorial_loop2   is 9.332622e+157 
## Factorial_reduce2 is 9.332622e+157 
## Factorial_func2   is 9.332622e+157 
## Factorial_mem2    is 9.332622e+157

all.equal(
  smp %>% Factorial_loop1,
  smp %>% Factorial_reduce1, 
  smp %>% Factorial_func1, 
  smp %>% Factorial_mem1, 
  smp %>% Factorial_loop2, 
  smp %>% Factorial_reduce2, 
  smp %>% Factorial_func2, 
  smp %>% Factorial_mem2)
## [1] TRUE

microbenchmark(
  smp %>% Factorial_loop1,
  smp %>% Factorial_reduce1, 
  smp %>% Factorial_func1, 
  smp %>% Factorial_mem1, 
  smp %>% Factorial_loop2, 
  smp %>% Factorial_reduce2, 
  smp %>% Factorial_func2, 
  smp %>% Factorial_mem2)
## Unit: microseconds
##                       expr     min       lq     mean   median       uq        max neval cld
##    smp %>% Factorial_loop1  66.714  75.4380 127.9314  84.1620  97.7615   3691.323   100 a  
##  smp %>% Factorial_reduce1 295.593 331.5160 411.2545 359.2275 414.6520   1589.840   100  bc
##    smp %>% Factorial_func1 123.678 140.6120 221.8747 154.2115 177.0485   2035.796   100 ab 
##     smp %>% Factorial_mem1 133.427 148.8230 183.9351 162.4225 186.7985    661.493   100 a  
##    smp %>% Factorial_loop2  64.661  78.2605 110.4627  89.5505 106.4860    584.002   100 a  
##  smp %>% Factorial_reduce2 297.646 329.4640 487.8213 353.5830 438.0015   5153.893   100   c
##    smp %>% Factorial_func2 118.032 132.9150 251.1518 145.7440 171.6595   4883.446   100 ab 
##     smp %>% Factorial_mem2  61.583  73.1285 223.1320  80.8265  99.8140  10736.294   100 ab


## fatorial 1000
fact_tbl <- c(rep(NA, 1000))
microbenchmark(
  1000 %>% Factorial_loop1,
  1000 %>% Factorial_reduce1, 
  1000 %>% Factorial_func1, 
  1000 %>% Factorial_mem1, 
  1000 %>% Factorial_loop2, 
  1000 %>% Factorial_reduce2, 
  1000 %>% Factorial_func2, 
  1000 %>% Factorial_mem2)
## Unit: microseconds
##                        expr     min        lq      mean    median       uq       max neval cld
##    1000 %>% Factorial_loop1 102.637  129.0655  204.7601  149.8495  197.062  1545.193   100 a  
##  1000 %>% Factorial_reduce1 874.976 1026.3660 1268.0129 1108.7310 1229.585  4656.619   100  bc
##    1000 %>% Factorial_func1 689.205  847.5215 1204.4244  980.9490 1286.293  4563.733   100  bc
##     1000 %>% Factorial_mem1 781.577  933.9925 1428.7468 1103.0860 1333.762  9419.468   100   c
##    1000 %>% Factorial_loop2 102.124  129.5785  180.3069  147.7970  190.647   956.573   100 a  
##  1000 %>% Factorial_reduce2 900.122 1000.7065 1282.0536 1081.0195 1306.820  5725.065   100  bc
##    1000 %>% Factorial_func2 625.570  738.2140 1102.4499  859.8380 1124.640  6487.655   100  b 
##     1000 %>% Factorial_mem2  68.253   91.8595  127.2438  108.7950  137.533   339.727   100 a
