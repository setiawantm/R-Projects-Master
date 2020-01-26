Evaluating Regression Models using Cross Validation and Bootstrap
================
Setiawan
November 25, 2018

Linear Regression
-----------------

Let the response Y be Distopia, and consider the numeric variables Economy, Family, Health, Freedom, Trust, and Generosity as the predictor variables; call these x1,x2,...,x6, respectively. For the purpose of predicting the value of Y , we will use linear regression models of the form yi = β0 + β1x1i + β2x2i +···+ β6x6i + Ei where the Ei are IID N(0,σ2), i = 1,2,...,n.

We will use Cross Validation and Bootstrap methods to evaluate the regression model.

### Importing Data

``` r
library(glmnet)
```

    ## Loading required package: Matrix

    ## Loaded glmnet 3.0-2

``` r
library(boot)
dta <- read.csv("happiness.csv")
colnames(dta) <- c("Country", "Region", "Religion", "Religious", "Economy",
                   "Family", "Health", "Freedom", "Trust", "Generosity", "Dystopia")
dta=dta[,5:11]
summary(dta)
```

    ##     Economy           Family           Health          Freedom      
    ##  Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000  
    ##  1st Qu.:0.6702   1st Qu.:0.6418   1st Qu.:0.3829   1st Qu.:0.2575  
    ##  Median :1.0278   Median :0.8414   Median :0.5966   Median :0.3975  
    ##  Mean   :0.9539   Mean   :0.7936   Mean   :0.5576   Mean   :0.3710  
    ##  3rd Qu.:1.2796   3rd Qu.:1.0215   3rd Qu.:0.7299   3rd Qu.:0.4845  
    ##  Max.   :1.8243   Max.   :1.1833   Max.   :0.9528   Max.   :0.6085  
    ##      Trust           Generosity        Dystopia     
    ##  Min.   :0.00000   Min.   :0.0000   Min.   :0.8179  
    ##  1st Qu.:0.06126   1st Qu.:0.1546   1st Qu.:2.0317  
    ##  Median :0.10547   Median :0.2225   Median :2.2907  
    ##  Mean   :0.13762   Mean   :0.2426   Mean   :2.3258  
    ##  3rd Qu.:0.17554   3rd Qu.:0.3119   3rd Qu.:2.6646  
    ##  Max.   :0.50521   Max.   :0.8197   Max.   :3.8377

### Cross Validation

In this case we will use leave one out cross validation (LOOCV). It is a K-fold cross validation taken to its logical extreme, with K equal to N, the number of data points in the set. That means that N separate times, the function approximator is trained on all the data except for one point and a prediction is made for that point. As before the average error is computed and used to evaluate the model.

``` r
err2=matrix(nrow=157,ncol=1)
for (i in 1:157) {
  dta2=dta[-i,]
  lm=lm(dta2$Dystopia~dta2$Economy+dta2$Family+dta2$Health+dta2$Freedom+dta2$Trust+dta2$Generosity)
  data=dta[i,]
  y_hat=lm$coefficients[1]+lm$coefficients[2]*data[,1]+lm$coefficients[3]*data[,2]+lm$coefficients[4]*data[,3]+lm$coefficients[5]*data[,4]+lm$coefficients[6]*data[,5]+lm$coefficients[7]*data[,6]
  y=data[,7]
  err2[i,]=(y-y_hat)^2
}
cv=1/157*sum(err2)
cat('The Cross Validation estimate of mean squared error is: ',cv)
```

    ## The Cross Validation estimate of mean squared error is:  0.3069102

### Bootstrap

The bootstrap is a computational resampling technique for finding standard errors (and in fact other things such as confidence intervals), with the only input being the procedure for calculating the estimate (or estimator) of interest on a sample of data.

``` r
boot.fn=function(data,index)
  return(coef(lm(Dystopia~Economy+Family+Health+Freedom+Trust+Generosity,data=data,subset=index)))
boot.fn(dta,1:157)
```

    ## (Intercept)     Economy      Family      Health     Freedom       Trust 
    ##  2.19027266 -0.27859272  0.22974215  0.43657303  0.51379355 -0.08077785 
    ##  Generosity 
    ## -0.84072432

``` r
set.seed(1)
B=1000
alpha=matrix(nrow=B, ncol=7)
for (i in 1:B) {
  alpha[i,]=boot.fn(dta,sample(157,157,replace=T))
}
SE=matrix(nrow=1,ncol=7)
original=matrix(nrow=1,ncol=7)
for (i in 1:7) {
  SE[,i]=sd(alpha[,i])
  original[,i]=mean(alpha[,i])
}
boot(dta,boot.fn,B)
```

    ## 
    ## ORDINARY NONPARAMETRIC BOOTSTRAP
    ## 
    ## 
    ## Call:
    ## boot(data = dta, statistic = boot.fn, R = B)
    ## 
    ## 
    ## Bootstrap Statistics :
    ##        original        bias    std. error
    ## t1*  2.19027266  0.0001944355   0.1428992
    ## t2* -0.27859272  0.0133339043   0.2306846
    ## t3*  0.22974215 -0.0149264726   0.2200469
    ## t4*  0.43657303  0.0001356457   0.3746220
    ## t5*  0.51379355 -0.0205631759   0.4245645
    ## t6* -0.08077785  0.0219764942   0.5772025
    ## t7* -0.84072432  0.0249900286   0.3940615

``` r
original
```

    ##          [,1]       [,2]      [,3]      [,4]      [,5]       [,6]      [,7]
    ## [1,] 2.198361 -0.2800238 0.2090495 0.4532074 0.5050705 -0.0188421 -0.853342

``` r
cat('Standard Error: ', SE)
```

    ## Standard Error:  0.1369172 0.2288693 0.2109484 0.3661417 0.4301589 0.5882178 0.416444

Lasso Regression
----------------

Lasso stands for Least Absolute Shrinkage and Selection Operator. Lasso regression is a type of linear regression that uses shrinkage. Shrinkage is where data values are shrunk towards a central point, like the mean. The lasso procedure encourages simple, sparse models (i.e. models with fewer parameters). This particular type of regression is well-suited for models showing high levels of muticollinearity or when you want to automate certain parts of model selection, like variable selection/parameter elimination.

``` r
x=model.matrix(Dystopia~.,dta)[,-1]
y=dta$Dystopia
x2 <- scale(x)

set.seed(10)
train=sample(1:nrow(x2), nrow(x2)/2)
test=(-train)
y.test=y[test]

lasso.mod=glmnet(x2[train,],y[train],alpha=1)
plot(lasso.mod,xvar="lambda")
```

![](Evaluating-Regression-Model_files/figure-markdown_github/lasso-1.png)

``` r
cv.out=cv.glmnet(x2[train,],y[train],alpha=1)
cv.out$lambda.min
```

    ## [1] 0.07209913

``` r
plot(cv.out)
```

![](Evaluating-Regression-Model_files/figure-markdown_github/lasso-2.png)

``` r
lasso.mod=glmnet(x2[test,],y[test],alpha=1,lambda=cv.out$lambda.min)
lasso.pred=predict(lasso.mod,newx=x2[test,])
mean((lasso.pred-y.test)^2)
```

    ## [1] 0.3149616

``` r
out=glmnet (x2,y,alpha =1, lambda =cv.out$lambda.min)
lasso.coef=predict (out ,type ="coefficients",s=cv.out$lambda.min )[1:7 ,]
lasso.coef
```

    ## (Intercept)     Economy      Family      Health     Freedom       Trust 
    ##    2.325807    0.000000    0.000000    0.000000    0.000000    0.000000 
    ##  Generosity 
    ##    0.000000

``` r
lm=lm(dta$Dystopia~dta$Economy+dta$Family+dta$Health+dta$Freedom+dta$Trust+dta$Generosity)
lm$coefficients
```

    ##    (Intercept)    dta$Economy     dta$Family     dta$Health    dta$Freedom 
    ##     2.19027266    -0.27859272     0.22974215     0.43657303     0.51379355 
    ##      dta$Trust dta$Generosity 
    ##    -0.08077785    -0.84072432

The selected value of lambda is 0.072. Note that, since the value of lambda with the lowest MSE is the largest one tried, we may want to specify the range of lambda to use rather than letting R choose for us. With the selected lambda, all the coefficients are shrunk to zero. Thus, we can apparently improve our predictive accuracy by ignoring all the predictor variables and using an overall mean from the training set as our prediction for every individual.
