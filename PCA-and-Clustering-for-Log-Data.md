PCA and Clustering for Log Data
================
Setiawan
November 11, 2018

Principal Component Analysis for Well Log Data
----------------------------------------------

Principal Component Analysis (PCA) Principal component analysis is done by selecting a new axes that represents maximum variability. The new axes depend on the eigenvectors of the covariance matrix. The first principal component always explains the highest proportion of the variance, followed by the second principal component and so on and so forth. The principal components can be determined by multiplying the original variables with eigenvectors of the covariance matrix. In other words, the principal components are the linear combinations of the original variables, with the eigenvectors of the covariance matrix which are the coefficients of the linear combinations.

PCA can be used to reduce dimension to reduce the complexity of an analysis. This can be done by selecting few first principal components and discard the remaining others. The nature of PCA is that the first principal component always explains the greatest variance, and the proportion of variance explained is lower for the next principal components so that the k-th principal component explains more variance than (k+1)-th principal component.

### Importing and Normalizing (Scaling) Data

There are 7 well log variables data that will be imported. The data is normalized (centered and scaled) to ensure that the analysis is not dominated by a certain variable which has a large variance and therefore overshadowing the “more important” variables in the process.

``` r
dta=read.delim("test.dat")
head(dta)
```

    ##     Well Zone DEPTH LT   GR log10.LLD. log10.MSFL.    DT RHOB NPHI  PEF  POR
    ## 1 well_1    5  6224  3 7.44     2.0790      1.8232 59.31 2.60 0.06 8.59  9.3
    ## 2 well_1    5  6225  3 5.21     2.1180      1.9281 57.63 2.59 0.07 8.00  8.9
    ## 3 well_1    5  6226  3 5.59     2.1902      1.7045 60.25 2.57 0.07 8.66  7.7
    ## 4 well_1    5  6227  3 5.98     2.1210      2.0332 61.69 2.54 0.07 8.67 11.7
    ## 5 well_1    5  6228  7 7.86     2.0646      1.6133 63.19 2.50 0.10 8.48 12.5
    ## 6 well_1    5  6229  7 7.82     2.0646      1.4921 62.00 2.52 0.11 9.70 12.7
    ##       Kg ln.Kg.
    ## 1  1.472  0.387
    ## 2  2.027  0.707
    ## 3 10.879  2.387
    ## 4  9.562  2.258
    ## 5 15.802  2.760
    ## 6 11.415  2.435

``` r
#Normalise data
dta$GR=scale(dta$GR,center=TRUE, scale=TRUE)
dta$log10.LLD.=scale(dta$log10.LLD.,center=TRUE, scale=TRUE)
dta$log10.MSFL.=scale(dta$log10.MSFL.,center=TRUE, scale=TRUE)
dta$DT=scale(dta$DT,center=TRUE, scale=TRUE)
dta$RHOB=scale(dta$RHOB,center=TRUE, scale=TRUE)
dta$NPHI=scale(dta$NPHI,center=TRUE, scale=TRUE)
dta$PEF=scale(dta$PEF,center=TRUE, scale=TRUE)
```

### Finding Covariance and Eigenvectors

``` r
num_vars <- c("GR", "log10.LLD.", "log10.MSFL.", "DT", "RHOB", "NPHI", "PEF")
X <- as.matrix(dta[, num_vars])
p <- ncol(X)
R=cor(X)
eigen(R)
```

    ## eigen() decomposition
    ## $values
    ## [1] 3.8007399 1.3755159 0.6851151 0.5578546 0.3382135 0.1354808 0.1070802
    ## 
    ## $vectors
    ##             [,1]        [,2]       [,3]        [,4]        [,5]        [,6]
    ## [1,]  0.12242301  0.62792335  0.7612477  0.02732127  0.09576185 -0.01530463
    ## [2,]  0.37943676 -0.29311789  0.1193244 -0.56728451  0.65351689 -0.07159533
    ## [3,]  0.41230447 -0.12680905  0.1443947 -0.48342999 -0.74172612 -0.04572902
    ## [4,] -0.46950854 -0.14017924  0.1981208 -0.16062647 -0.06763054 -0.80478119
    ## [5,]  0.46430803  0.08936015 -0.2054320  0.33959289  0.07859498 -0.55339959
    ## [6,] -0.47592539 -0.08925587  0.1243482 -0.28277087  0.03956257  0.17188712
    ## [7,]  0.09230484 -0.68418679  0.5371192  0.47186197 -0.03565743  0.09528743
    ##             [,7]
    ## [1,] -0.03279824
    ## [2,]  0.04300463
    ## [3,] -0.08438795
    ## [4,]  0.20642714
    ## [5,] -0.55360535
    ## [6,] -0.79937237
    ## [7,] -0.04320083

``` r
svd=svd(X)
```

### PCA

``` r
pca <- prcomp(X) # centers by default
pca
```

    ## Standard deviations (1, .., p=7):
    ## [1] 1.9495486 1.1728239 0.8277168 0.7468966 0.5815613 0.3680772 0.3272311
    ## 
    ## Rotation (n x k) = (7 x 7):
    ##                     PC1         PC2        PC3         PC4         PC5
    ## GR           0.12242301 -0.62792335  0.7612477 -0.02732127  0.09576185
    ## log10.LLD.   0.37943676  0.29311789  0.1193244  0.56728451  0.65351689
    ## log10.MSFL.  0.41230447  0.12680905  0.1443947  0.48342999 -0.74172612
    ## DT          -0.46950854  0.14017924  0.1981208  0.16062647 -0.06763054
    ## RHOB         0.46430803 -0.08936015 -0.2054320 -0.33959289  0.07859498
    ## NPHI        -0.47592539  0.08925587  0.1243482  0.28277087  0.03956257
    ## PEF          0.09230484  0.68418679  0.5371192 -0.47186197 -0.03565743
    ##                     PC6         PC7
    ## GR          -0.01530463 -0.03279824
    ## log10.LLD.  -0.07159533  0.04300463
    ## log10.MSFL. -0.04572902 -0.08438795
    ## DT          -0.80478119  0.20642714
    ## RHOB        -0.55339959 -0.55360535
    ## NPHI         0.17188712 -0.79937237
    ## PEF          0.09528743 -0.04320083

``` r
summary(pca)
```

    ## Importance of components:
    ##                          PC1    PC2     PC3     PC4     PC5     PC6    PC7
    ## Standard deviation     1.950 1.1728 0.82772 0.74690 0.58156 0.36808 0.3272
    ## Proportion of Variance 0.543 0.1965 0.09787 0.07969 0.04832 0.01935 0.0153
    ## Cumulative Proportion  0.543 0.7395 0.83734 0.91703 0.96535 0.98470 1.0000

``` r
names(pca)
```

    ## [1] "sdev"     "rotation" "center"   "scale"    "x"

### Variance Explained

``` r
variance_explained <- pca$sdev^2/sum(pca$sdev^2)
plot(variance_explained,type="b",xlab="PC",ylab="Variance Explained")
```

![](PCA-and-Clustering-for-Log-Data_files/figure-markdown_github/var-1.png)

``` r
plot(cumsum(variance_explained),type="b",xlab="PC",ylab="Cumulative Variance Explained")
abline(0.9,0, col=2)
```

![](PCA-and-Clustering-for-Log-Data_files/figure-markdown_github/var-2.png) The PCA result can be observed at figures above. In this case, we are going to use 4 principal components in the next step of this project since they explain 90% of the total variance, and there will be negligible loss of information.

### Plot Pairs

``` r
forpair=matrix(NA,nrow=904, ncol=14)
forpair[,1:7]=X
forpair[,8:14]=pca$x
pairs(forpair[,1:9], labels=c("GR","LLD","MSFL", "DT", "RHOB", "NPHI", "PEF", "PC1", "PC2"))
```

![](PCA-and-Clustering-for-Log-Data_files/figure-markdown_github/pairs-1.png)

``` r
plot(pca$x[,1],X[,6],xlab="PC1",ylab="NPHI",main="Relationship between PC1 and NPHI")
```

![](PCA-and-Clustering-for-Log-Data_files/figure-markdown_github/pairs-2.png)

``` r
plot(pca$x[,2],X[,1],xlab="PC2",ylab="GR",main="Relationship between PC2 and GR")
```

![](PCA-and-Clustering-for-Log-Data_files/figure-markdown_github/pairs-3.png)

``` r
#function to plot
jpeg('pairs.jpg')
pairs(forpair[,1:9], labels=c("GR","LLD","MSFL", "DT", "RHOB", "NPHI", "PEF", "PC1", "PC2"))
dev.off()
```

    ## png 
    ##   2

Principal components have no physical meaning. However, we can interpret them based on the relationship of the principal components and the original variables. Figure above shows the relationship between PC1 and PC2 (the first 2 principal components) with the 7 variables. The relationship follows the value of coefficients (correlations) which are the eigenvectors showed earlier. Based on the figure, the first principal component has a monotonic increasing relationship with the porosity and the second principal component has a monotonic decreasing relationship with the gamma ray and photoelectric log which tell us the lithology. In other words, the first principal component “explains” the tightness of the rock and the second principal component “explains” the lithology of the rock (GR).

Clustering
----------

The methodology used to do the clustering is the k-means clustering. K-mean clustering is a method to divide data into several clusters which need to be determined beforehand. It divides data into clusters by minimizing the distance of each data into the center of each cluster. After determining the number of clusters, each observations is assigned to a random cluster. The mean of each clusters is then calculated, and assign each observations to the cluster that has the closest mean (centroid), and iterate this step until there is no change in cluster assignments. This algorithm may find the local minimum instead of the global minimum. Therefore, it is good to perform k-mean clustering several times with different random cluster assignments. The one with the lowest Euclidean distance is assumed to give the global minimum and therefore it is used as the clustering result. In this project, the k-mean clustering is performed 100 times. Number of clusters will be varied from 4-6 to determine what number of clusters is the most appropriate for the clustering. Boxplots of the logs will be used to determine the appropriate number of clusters.

### K-means Clustering

``` r
# K-mean clustering 4 clusters
hc.complete=hclust(dist(pca$x), method="complete")
plot(hc.complete,main="Complete Linkage", xlab="", sub="", cex=.9)
```

![](PCA-and-Clustering-for-Log-Data_files/figure-markdown_github/4clus-1.png)

``` r
km.out4=kmeans(pca$x[,1:4],4,nstart=100)
plot(pca$x[,1:2], col=(km.out4$cluster+1), main="K-Means Clustering Results", pch=20, cex=2)
```

![](PCA-and-Clustering-for-Log-Data_files/figure-markdown_github/4clus-2.png)

``` r
boxplot(dta$GR~km.out4$cluster, main="boxplot GR with 4 Clusters")
```

![](PCA-and-Clustering-for-Log-Data_files/figure-markdown_github/4clus-3.png)

``` r
boxplot(dta$log10.LLD.~km.out4$cluster, main="boxplot LLD with 4 Clusters")
```

![](PCA-and-Clustering-for-Log-Data_files/figure-markdown_github/4clus-4.png)

``` r
boxplot(dta$log10.MSFL.~km.out4$cluster, main="boxplot MSFL with 4 Clusters")
```

![](PCA-and-Clustering-for-Log-Data_files/figure-markdown_github/4clus-5.png)

``` r
boxplot(dta$DT~km.out4$cluster, main="boxplot DT with 4 Clusters")
```

![](PCA-and-Clustering-for-Log-Data_files/figure-markdown_github/4clus-6.png)

``` r
boxplot(dta$RHOB~km.out4$cluster, main="boxplot RHOB with 4 Clusters")
```

![](PCA-and-Clustering-for-Log-Data_files/figure-markdown_github/4clus-7.png)

``` r
boxplot(dta$NPHI~km.out4$cluster, main="boxplot NPHI with 4 Clusters")
```

![](PCA-and-Clustering-for-Log-Data_files/figure-markdown_github/4clus-8.png)

``` r
boxplot(dta$PEF~km.out4$cluster, main="boxplot PEF with 4 Clusters")
```

![](PCA-and-Clustering-for-Log-Data_files/figure-markdown_github/4clus-9.png)

``` r
# K-mean clustering 5 clusters
km.out5=kmeans(pca$x[,1:4],5,nstart=100)
plot(pca$x[,1:2], col=(km.out5$cluster+1), main="K-Means Clustering Results", pch=20, cex=2)
```

![](PCA-and-Clustering-for-Log-Data_files/figure-markdown_github/5clus-1.png)

``` r
boxplot(dta$GR~km.out5$cluster, main="boxplot GR with 5 Clusters")
```

![](PCA-and-Clustering-for-Log-Data_files/figure-markdown_github/5clus-2.png)

``` r
boxplot(dta$log10.LLD.~km.out5$cluster, main="boxplot LLD with 5 Clusters")
```

![](PCA-and-Clustering-for-Log-Data_files/figure-markdown_github/5clus-3.png)

``` r
boxplot(dta$log10.MSFL.~km.out5$cluster, main="boxplot MSFL with 5 Clusters")
```

![](PCA-and-Clustering-for-Log-Data_files/figure-markdown_github/5clus-4.png)

``` r
boxplot(dta$DT~km.out5$cluster, main="boxplot DT with 5 Clusters")
```

![](PCA-and-Clustering-for-Log-Data_files/figure-markdown_github/5clus-5.png)

``` r
boxplot(dta$RHOB~km.out5$cluster, main="boxplot RHOB with 5 Clusters")
```

![](PCA-and-Clustering-for-Log-Data_files/figure-markdown_github/5clus-6.png)

``` r
boxplot(dta$NPHI~km.out5$cluster, main="boxplot NPHI with 5 Clusters")
```

![](PCA-and-Clustering-for-Log-Data_files/figure-markdown_github/5clus-7.png)

``` r
boxplot(dta$PEF~km.out5$cluster, main="boxplot PEF with 5 Clusters")
```

![](PCA-and-Clustering-for-Log-Data_files/figure-markdown_github/5clus-8.png)

``` r
# K-mean clustering 6 clusters
km.out6=kmeans(pca$x[,1:4],7,nstart=100)
plot(pca$x[,1:2], col=(km.out6$cluster+1), main="K-Means Clustering Results", pch=20, cex=2)
```

![](PCA-and-Clustering-for-Log-Data_files/figure-markdown_github/6clus-1.png)

``` r
boxplot(dta$GR~km.out6$cluster, main="GR with 6 Clusters")
```

![](PCA-and-Clustering-for-Log-Data_files/figure-markdown_github/6clus-2.png)

``` r
boxplot(dta$log10.LLD.~km.out6$cluster, main="LLD with 6 Clusters")
```

![](PCA-and-Clustering-for-Log-Data_files/figure-markdown_github/6clus-3.png)

``` r
boxplot(dta$log10.MSFL.~km.out6$cluster, main="MSFL with 6 Clusters")
```

![](PCA-and-Clustering-for-Log-Data_files/figure-markdown_github/6clus-4.png)

``` r
boxplot(dta$DT~km.out6$cluster, main="DT with 6 Clusters")
```

![](PCA-and-Clustering-for-Log-Data_files/figure-markdown_github/6clus-5.png)

``` r
boxplot(dta$RHOB~km.out6$cluster, main="RHOB with 6 Clusters")
```

![](PCA-and-Clustering-for-Log-Data_files/figure-markdown_github/6clus-6.png)

``` r
boxplot(dta$NPHI~km.out6$cluster, main="NPHI with 6 Clusters")
```

![](PCA-and-Clustering-for-Log-Data_files/figure-markdown_github/6clus-7.png)

``` r
boxplot(dta$PEF~km.out6$cluster, main="PEF with 6 Clusters")
```

![](PCA-and-Clustering-for-Log-Data_files/figure-markdown_github/6clus-8.png)

``` r
jpeg('clusters.jpg')
par(mfrow=c(1,3))
plot(pca$x[,1:2], col=(km.out4$cluster+1), main="4 Clusters", pch=20, cex=2)
plot(pca$x[,1:2], col=(km.out5$cluster+1), main="5 Clusters", pch=20, cex=2)
plot(pca$x[,1:2], col=(km.out6$cluster+1), main="6 Clusters", pch=20, cex=2)
dev.off()
```

    ## png 
    ##   2

### Boxplots for Variables for Each Clusters

Boxplots of the logs are used to determine the appropriate number of clusters. The log interpretations are done qualitatively, and further analysis is needed to quantify the parameters, for example, porosity and saturation. In the end, consulting the results with geoscientists will be a good idea to do.

``` r
jpeg('GRPEF.jpg')
par(mfrow=c(2,3))
boxplot(dta$GR~km.out4$cluster, main="GR with 4 Clusters")
boxplot(dta$GR~km.out5$cluster, main="GR with 5 Clusters")
boxplot(dta$GR~km.out6$cluster, main="GR with 6 Clusters")
boxplot(dta$PEF~km.out4$cluster, main="PEF with 4 Clusters")
boxplot(dta$PEF~km.out5$cluster, main="PEF with 5 Clusters")
boxplot(dta$PEF~km.out6$cluster, main="PEF with 6 Clusters")
dev.off()
```

    ## png 
    ##   2

``` r
jpeg('porosity.jpg')
par(mfrow=c(3,3))
boxplot(dta$DT~km.out4$cluster, main="DT with 4 Clusters")
boxplot(dta$DT~km.out5$cluster, main="DT with 5 Clusters")
boxplot(dta$DT~km.out6$cluster, main="DT with 6 Clusters")
boxplot(dta$NPHI~km.out4$cluster, main="NPHI with 4 Clusters")
boxplot(dta$NPHI~km.out5$cluster, main="NPHI with 5 Clusters")
boxplot(dta$NPHI~km.out6$cluster, main="NPHI with 6 Clusters")
boxplot(dta$RHOB~km.out4$cluster, main="RHOB with 4 Clusters")
boxplot(dta$RHOB~km.out5$cluster, main="RHOB with 5 Clusters")
boxplot(dta$RHOB~km.out6$cluster, main="RHOB with 6 Clusters")
dev.off()
```

    ## png 
    ##   2

``` r
jpeg('resistivity.jpg')
par(mfrow=c(2,3))
boxplot(dta$log10.LLD.~km.out4$cluster, main="LLD with 4 Clusters")
boxplot(dta$log10.LLD.~km.out5$cluster, main="LLD with 5 Clusters")
boxplot(dta$log10.LLD.~km.out6$cluster, main="LLD with 6 Clusters")
boxplot(dta$log10.MSFL.~km.out4$cluster, main="MSFL with 4 Clusters")
boxplot(dta$log10.MSFL.~km.out5$cluster, main="MSFL with 5 Clusters")
boxplot(dta$log10.MSFL.~km.out6$cluster, main="MSFL with 6 Clusters")
dev.off()
```

    ## png 
    ##   2

Based on the boxplots, the qualitative interpretations below are made. Note that the high value of photoelectric is interpreted to be limestone and medium value to be dolomite, while the low value is sand. For 4 clusters, the characteristics of each clusters are: 1. Clean sand, High porosity, Low resistivity 2. Limestone, Medium porosity, Medium resistivity 3. Shaly sand/dolomite, tight 4. Shaly sand, Medium porosity, Medium resistivity

For 5 clusters, the characteristics of each clusters are: 1. Clean sand, medium porosity, medium resistivity 2. Shale, Low porosity 3. Clean sand, high porosity, low resistivity 4. Dolomite/limestone, tight 5. Limestone, medium porosity, medium resistivity

For 6 clusters, the characteristic of each clusters are: 1. Clean sand, medium porosity, medium resistivity 2. Shaly sand, low porosity, medium resistivity 3. Dolomite/limestone, tight 4. Limestone, medium porosity, medium resistivity 5. Clean sand, high porosity, low resistivity 6. Shale, low porosity

Compared to 4 clusters, dividing the observations into 5 clusters is better since by using 5 clusters, shale can be clearly differentiated from the other rocks. In 5 clusters, the best cluster to deliver fluid is the third cluster which is clean sand with high porosity. However, based on the resistivity, it is a water bearing zone. Therefore, the best target for production is the first cluster which has medium porosity and medium resistivity. Another potential target is the fifth cluster which is limestone with medium porosity and medium resistivity. Dividing the observations into 6 clusters gives better detail about the group. However, the best cluster to deliver fluid is still the clean sand with high porosity and a water bearing zone (fifth cluster). The potential target to produce are clean sand with medium porosity and medium resistivity, and limestone with medium porosity and medium resistivity. Because of the similar results, it is better to divide the observations into 5 clusters instead of 6 clusters. Once again, the log interpretations need to be discussed with geologist, and cautions are to be applied since resistivity is also a function of porosity and quantifying saturation is necessary to determine the hydrocarbon bearing zone.

Summary
-------

1.  4 Principal components (PC) are used in the clustering analysis, since 4 PCs explain 90% of the total variance.
2.  K-Mean clustering algorithm is used to divide the observations into clusters, and each clustering algorithm is repeated 100 times to find the global minimum.
3.  Comparing 4,5, and 6 clusters qualitatively, the most appropriate number of clusters is 5, since it differentiate between sand and shale, and that there is no significant difference between 5 and 6 clusters.
4.  The best clusters are cluster 1 and 5, which are sandstone with medium porosity, medium resistivity, and limestone with medium porosity, medium resistivity respectively. The analysis is done qualitatively and quantitative calculation (porosity, saturation) needs to be done with addition to discussion with geoscientists.
