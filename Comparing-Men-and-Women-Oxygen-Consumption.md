Comparing Men and Women Oxygen Consumption
================
Setiawan
September 21, 2018

### Importing Data

``` r
dta <- read.delim("oxygen.DAT", header = FALSE, sep = "")
colnames(dta) <- c("X_1", "X_2", "X_3", "X_4", "Gender")
attach(dta)
summary(dta)
```

    ##       X_1              X_2              X_3             X_4           Gender  
    ##  Min.   :0.1100   Min.   : 1.740   Min.   :1.710   Min.   :28.97   female:25  
    ##  1st Qu.:0.3000   1st Qu.: 4.555   1st Qu.:2.320   1st Qu.:37.82   male  :25  
    ##  Median :0.3400   Median : 5.110   Median :2.845   Median :43.05              
    ##  Mean   :0.3554   Mean   : 5.254   Mean   :3.001   Mean   :43.79              
    ##  3rd Qu.:0.4000   3rd Qu.: 5.985   3rd Qu.:3.545   3rd Qu.:49.72              
    ##  Max.   :0.6600   Max.   :11.050   Max.   :5.230   Max.   :63.30

### Calculating Means and Standard Deviations

``` r
X_bar_1 <- colMeans(dta[1:25, c(1, 3)])
X_bar_2 <- colMeans(dta[26:50, c(1, 3)])
S_1 <- var(dta[1:25, c(1, 3)])
S_2 <- var(dta[26:50, c(1, 3)])

n_1=n_2=25
p=2
```

### Defining the Hypothesis

The hypothesis that will be tested is that the mean of oxygen volumes while resting and strenous exercise between men and women are the same Testing H0: X\_bar\_1 - X\_bar\_2 = 0

### Statistical Testing

#### Assuming Equal Variances

``` r
S_po <- ((n_1 - 1) * S_1 + (n_2 - 1) * S_2) / (n_1 + n_2 - 2)

T2 <- (X_bar_1 - X_bar_2) %*% solve(S_po * (1 / n_1 + 1 / n_2)) %*% (X_bar_1 - X_bar_2)
p_value <- 1 - pf((n_1 + n_2 - p - 1) / ((n_1 + n_2 - 2) * p) * T2, p, n_1 + n_2 - p - 1)
qf(0.95, p, n_1 + n_2 - p - 1)
```

    ## [1] 3.195056

T2=81.7, p-value=7.17e-11&lt;0.05, Therefore H0 is rejected.

#### Assuming Unequal Variances

``` r
SS_1 <- S_1 %*% solve(S_1 / n_1 + S_2 / n_2) / n_1
SS_2 <- S_2 %*% solve(S_1 / n_1 + S_2 / n_2) / n_2
nu <- (p + p ^ 2) / ((sum(diag(SS_1 ^ 2)) + sum(diag(SS_1)) ^ 2) / n_1 + 
                       (sum(diag(SS_2 ^ 2)) + sum(diag(SS_2)) ^ 2) / n_2)
T2 <- (X_bar_1 - X_bar_2) %*% solve(S_1 / n_1 + S_2 / n_2) %*% (X_bar_1 - X_bar_2)
p_value <- 1 - pf((nu - p + 1) / (nu * p) * T2, p, nu - p + 1)

X_bar_1[1] - X_bar_2[1] + c(-1, 1) * qt(1 - 0.05 / (2 * p), nu) * 
  sqrt(S_1[1, 1] / n_1 + S_2[1, 1] / n_2)
```

    ## [1] 0.02345611 0.14374389

``` r
X_bar_1[2] - X_bar_2[2] + c(-1, 1) * qt(1 - 0.05 / (2 * p), nu) * 
  sqrt(S_1[2, 2] / n_1 + S_2[2, 2] / n_2)
```

    ## [1] 1.020673 1.724127

T2=81.7, p-value=9.69e-11&lt;0.05, Therefore H0 is rejected.

#### Assuming Normal Distribution

``` r
T2 <- (X_bar_1 - X_bar_2) %*% solve((S_1 * 1 / n_1) + (S_2 * 1 / n_2)) %*% (X_bar_1 - X_bar_2)
p_value <- 1 - pchisq((n_1 + n_2 - p - 1) / ((n_1 + n_2 - 2) * p) * T2, p)
qchisq(0.95, p)
```

    ## [1] 5.991465

T2=81.7, p-value=2.06e-09&lt;0.05, Therefore H0 is rejected.

### Conclusion

The 3 assumptions end with same result, which is H0 is rejected. The H0 that the mean of oxygen volumes while resting and strenous exercise between men and women are the same is rejected, and therefore there is no evidence that the mean of oxygen consumption between men and women are the same.
