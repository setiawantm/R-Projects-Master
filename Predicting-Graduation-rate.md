Predicting Graduation Rate
================
Setiawan
November 25, 2018

Predicting Graduation Rate using Regression Tree, Pruned Tree, and Random Forest Regression
-------------------------------------------------------------------------------------------

### Introduction

This project is done to predict Graduation Rate based on datasets from colleges. 388 datasets are used as training sets, and the rests are used as test datasets. The predictions used are Regression Tree, Pruned Tree and Random Forest Regression. Using the 388 training sets, the models are trained, after that they are tested using test datasets, and finally mean squared error for every models is calculated to determine which model is the best option to predict the graduation rate.

### Histogram for Graduation Rate

``` r
library(ISLR)
library(glmnet)
```

    ## Loading required package: Matrix

    ## Loaded glmnet 3.0-2

``` r
library(boot)
library(tree)
library(randomForest)
```

    ## randomForest 4.6-14

    ## Type rfNews() to see new features/changes/bug fixes.

``` r
attach(College)
hist(College$Grad.Rate)
```

![](Predicting-Graduation-rate_files/figure-markdown_github/hist-1.png)

### Regression Tree

We will select 388 colleges data for training data randomly. After that we will train a regression tree using these training datasets.

``` r
set.seed(1)
train=sample(1:nrow(College), 388)
test=(-train)
tree.college=tree(Grad.Rate~.,College,subset=train)
summary(tree.college)
```

    ## 
    ## Regression tree:
    ## tree(formula = Grad.Rate ~ ., data = College, subset = train)
    ## Variables actually used in tree construction:
    ##  [1] "Outstate"    "perc.alumni" "Personal"    "Top25perc"   "S.F.Ratio"  
    ##  [6] "Top10perc"   "Accept"      "Books"       "F.Undergrad" "Enroll"     
    ## Number of terminal nodes:  17 
    ## Residual mean deviance:  118.2 = 43860 / 371 
    ## Distribution of residuals:
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ## -35.410  -6.018  -0.220   0.000   6.409  40.590

``` r
plot(tree.college)
text(tree.college,cex=0.7)
```

![](Predicting-Graduation-rate_files/figure-markdown_github/tree-1.png)

#### Predicting Graduation Rate using Test Datasets

``` r
result=predict(tree.college,newdata=College[test,])
gradrate=College[test,"Grad.Rate"]
plot(result,gradrate)
abline(0,1)
```

![](Predicting-Graduation-rate_files/figure-markdown_github/predtree-1.png)

``` r
mean((result-gradrate)^2)
```

    ## [1] 217.078

MSE = 217.078. The prediction accuracy is not very good, however it is nice to have because of its simplicity.

### Pruned Tree

We will now obtain a pruned version of the tree. After that, we will use cross-validation to assess what an appropriate value of the cost-complexity parameter is and compute a pruned tree.

``` r
cv.college=cv.tree(tree.college)
names(cv.college)
```

    ## [1] "size"   "dev"    "k"      "method"

``` r
plot(cv.college$size,cv.college$dev,type="b")
```

![](Predicting-Graduation-rate_files/figure-markdown_github/prune-1.png)

``` r
plot(cv.college$k,cv.college$dev,type="p")
```

![](Predicting-Graduation-rate_files/figure-markdown_github/prune-2.png)

``` r
best.size <- cv.college$size[which(cv.college$dev==min(cv.college$dev))] # which size is better?
best.size
```

    ## [1] 5

``` r
k=cv.college$k[which(cv.college$dev==min(cv.college$dev))]
k
```

    ## [1] 3166.406

``` r
prune.college=prune.tree(tree.college,best=best.size)
plot(prune.college)
text(prune.college)
```

![](Predicting-Graduation-rate_files/figure-markdown_github/prune-3.png) In this case, we choose the value of the cost-complexity parameter to be 3166.406 which corresponds to a tree size of 5 terminal nodes (4 splits).

#### Predicting Graduation Rate using Pruned Tree

``` r
result2=predict(prune.college,newdata=College[test,])
plot(result2,gradrate)
abline(0,1)
```

![](Predicting-Graduation-rate_files/figure-markdown_github/predictprune-1.png)

``` r
mean((result2-gradrate)^2)
```

    ## [1] 201.5259

The new test MSE is 201.5259, which is lower than for the original tree. Typically, we would expect the pruned tree to have lower MSE (it was selected via CV to have low MSE), however, due to the randomness in selecting the test dataset there is no guarantee that the MSE will be lower.

### Random Forest Regression

Random forest is a Supervised Learning algorithm which uses ensemble learning method for classification and regression. Random forest is a bagging technique and not a boosting technique. The trees in random forests are run in parallel. There is no interaction between these trees while building the trees. It operates by constructing a multitude of decision trees at training time and outputting the class that is the mode of the classes (classification) or mean prediction (regression) of the individual trees.

``` r
rf.college=randomForest(Grad.Rate~.,College,subset=train,mtry=6,importance=TRUE)
rf.college
```

    ## 
    ## Call:
    ##  randomForest(formula = Grad.Rate ~ ., data = College, mtry = 6,      importance = TRUE, subset = train) 
    ##                Type of random forest: regression
    ##                      Number of trees: 500
    ## No. of variables tried at each split: 6
    ## 
    ##           Mean of squared residuals: 171.2752
    ##                     % Var explained: 43.18

``` r
plot(rf.college)
```

![](Predicting-Graduation-rate_files/figure-markdown_github/randforest-1.png) The number of trees should probably be set to some value between 100 and 200.

#### Predicting Graduation Rate using Random Forest Regression

``` r
College.test=College[-train,"Grad.Rate"] 
yhat.rf = predict(rf.college,newdata=College[-train,]) 
mean((yhat.rf-College.test)^2)
```

    ## [1] 162.6214

The MSE from random forest model is 160.141, which is much lower than regression tree and pruned tree.

### Fit a Random Forest Clasifier to Predict value of High (Graduation Rate&gt;70)

``` r
High=ifelse(Grad.Rate>70,"Yes","No")
College=data.frame(College,High)
Collegehigh.test=College[-train,]
High.test=High[-train]
rf.collegehigh=randomForest(High~.-Grad.Rate,College,subset=train)
rf.collegehigh
```

    ## 
    ## Call:
    ##  randomForest(formula = High ~ . - Grad.Rate, data = College,      subset = train) 
    ##                Type of random forest: classification
    ##                      Number of trees: 500
    ## No. of variables tried at each split: 4
    ## 
    ##         OOB estimate of  error rate: 23.97%
    ## Confusion matrix:
    ##      No Yes class.error
    ## No  208  33   0.1369295
    ## Yes  60  87   0.4081633

``` r
rf.predhigh=predict(rf.collegehigh,Collegehigh.test,type="class")
table(rf.predhigh,High.test)
```

    ##            High.test
    ## rf.predhigh  No Yes
    ##         No  210  61
    ##         Yes  26  92
