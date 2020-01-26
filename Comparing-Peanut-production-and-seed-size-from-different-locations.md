Comparing Peanut Production and Seed Size from Different Locations
================
Setiawan
September 21, 2018

### Introduction

The peanut dataset has two factors, which are geographical locations and varieties. For each crops, we have measurements of three weight variables: X1 = total yield, X2 = sound mature kernels, and X3 = seed size. In this project, we will test whether the mean vector for total yield and seed size varies by location.

### Importing Data

``` r
dta <- read.csv("peanut.csv")
dtasplit <- split(dta,dta$Location)
X_1 <- dtasplit$`1`
X_2 <- dtasplit$`2`

summary(dtasplit)
```

    ##   Length Class      Mode
    ## 1 5      data.frame list
    ## 2 5      data.frame list

### Checking Normality

``` r
## Checking Normality
qqnorm(X_1[, 3])
qqline(X_1[, 3])
```

![](Comparing-Peanut-production-and-seed-size-from-different-locations_files/figure-markdown_github/norm-1.png)

``` r
qqnorm(X_1[, 5])
qqline(X_1[, 5])
```

![](Comparing-Peanut-production-and-seed-size-from-different-locations_files/figure-markdown_github/norm-2.png)

``` r
qqnorm(X_2[, 3])
qqline(X_2[, 3])
```

![](Comparing-Peanut-production-and-seed-size-from-different-locations_files/figure-markdown_github/norm-3.png)

``` r
qqnorm(X_2[, 5])
qqline(X_2[, 5])
```

![](Comparing-Peanut-production-and-seed-size-from-different-locations_files/figure-markdown_github/norm-4.png)

``` r
X_bar_1 <- colMeans(X_1[, c(3, 5)])
X_bar_2 <- colMeans(X_2[, c(3, 5)])
S_1 <- var(X_1[, c(3, 5)])
S_2 <- var(X_2[, c(3, 5)])

n_1=n_2=6
p=2
```

Based on the Q-Q plots, we can safely assume that the data is normally distributed.

### Defining the Hypothesis

The hypothesis that will be tested is that the mean of peanut's total yields and seed sizes are the same for different locations. By testing this hypothesis, we can determine whether mean of total yield and seed size varies by locations.

Testing H0: X\_bar\_1 - X\_bar\_2 = 0

### Statistical Testing

#### Assuming equal variances and Normal distribution

``` r
S_po <- ((n_1 - 1) * S_1 + (n_2 - 1) * S_2) / (n_1 + n_2 - 2)

T2 <- (X_bar_1 - X_bar_2) %*% solve(S_po * (1 / n_1 + 1 / n_2)) %*% (X_bar_1 - X_bar_2)
p_value <- 1 - pf((n_1 + n_2 - p - 1) / ((n_1 + n_2 - 2) * p) * T2, p, n_1 + n_2 - p - 1)

cat('T2=',T2)
```

    ## T2= 1.839086

``` r
cat('p value=',p_value)
```

    ## p value= 0.4678087

T2=1.84, p-value=0.468&gt;0.05, therefore H0 is accepted.

#### Assuming unequal variances and Normal distribution

``` r
SS_1 <- S_1 %*% solve(S_1 / n_1 + S_2 / n_2) / n_1
SS_2 <- S_2 %*% solve(S_1 / n_1 + S_2 / n_2) / n_2
nu <- (p + p ^ 2) / ((sum(diag(SS_1 ^ 2)) + sum(diag(SS_1)) ^ 2) / n_1 + 
                       (sum(diag(SS_2 ^ 2)) + sum(diag(SS_2)) ^ 2) / n_2)
T2 <- (X_bar_1 - X_bar_2) %*% solve(S_1 / n_1 + S_2 / n_2) %*% (X_bar_1 - X_bar_2)
p_value <- 1 - pf((nu - p + 1) / (nu * p) * T2, p, nu - p + 1)
cat('T2=',T2)
```

    ## T2= 1.839086

``` r
cat('p value=',p_value)
```

    ## p value= 0.4573856

T2=1.84, p-value=0.457&gt;0.05, therefore H0 is accepted.

### Conclusion

Both equal and unequal variance assumption end with the same result that H0 is accepted. Therefore, mean of total yield and seed size does not vary with location.
