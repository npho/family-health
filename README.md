# family-health
Analysis of family health data from the National Vital Statistics System of the National Center for Health Statistics.

Used [MXNet](https://mxnet.apache.org) for artificial neural networks (ANNs), instructions for installing [here](https://mxnet.incubator.apache.org/versions/master/install/index.html). As of February 10, 2019 MXNet did not compile for R-3.5.2, downgrade to R-3.5.1 for compatibility.

```
cran <- getOption("repos")
cran["dmlc"] <- "https://apache-mxnet.s3-accelerate.dualstack.amazonaws.com/R/CRAN/"
options(repos = cran)
install.packages("mxnet")
```