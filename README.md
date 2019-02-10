# family-health
Analysis of family health data from the National Vital Statistics System of the National Center for Health Statistics.

All standard R packages used should be able to be installed directly from CRAN without issue. The only exception is MXNet for which additional hacks are detailed below.

Used [MXNet](https://mxnet.apache.org) for artificial neural networks (ANNs), instructions for installing [here](https://mxnet.incubator.apache.org/versions/master/install/index.html). For macOS the instructions say to use `homebrew` to install OpenCV. However, by default v4 gets installed and MXNet only recognized v3 when I tested. Install `opencv@3` then symbolically link the expected OpenCV path.

```r
cran <- getOption("repos")
cran["dmlc"] <- "https://apache-mxnet.s3-accelerate.dualstack.amazonaws.com/R/CRAN/"
options(repos = cran)
install.packages("mxnet")
```
