# family-health

by: npho3

Survey of supervised learning approaches to analysis of family health data from the National Vital Statistics System of the National Center for Health Statistics. See submitted paper `npho3-analysis.pdf` for full details.

## Getting Data

The data sets are (1) natality data from every recorded birth in the US in 2017 and (2) the linked mortality statistics from 2012 for every child under 1 year of age who died that year. Each can be processed with the pre-processing scripts in this repository, make sure to edit the scripts to point to the appropriate location of the original data on your local computer. The pre-processing scripts made sure the columns are of the appropriate R data structure (e.g., factor, numeric) as well as manually curating the features by removing redundant or medically irrelevant (to our hypothesis) ones. Alternatively, explore the `dat` folder within the repository for the cleaned and prepared R objects saved as `RData` files. Note: make sure `git lfs` is configured locally to handle these larger files.

## Library Setup

All standard R packages used should be able to be installed directly from CRAN without issue. The only exception is MXNet for which additional hacks are detailed below.

Used [MXNet](https://mxnet.apache.org) for artificial neural networks (ANNs), instructions for installing [here](https://mxnet.incubator.apache.org/versions/master/install/index.html). For macOS the instructions say to use `homebrew` to install OpenCV. However, by default v4 gets installed and MXNet only recognized v3 when I tested. Install `opencv@3` then symbolically link the expected OpenCV path.

```r
cran <- getOption("repos")
cran["dmlc"] <- "https://apache-mxnet.s3-accelerate.dualstack.amazonaws.com/R/CRAN/"
options(repos = cran)
install.packages("mxnet")
```

## Running Code

Each supervised learning approach lives within its own self-contained R script within the base repository. The code can be run by loaded an interactive R session then calling each script via `source()`. It wasn't explicitly tested but it's likely running it fully from the command line via `Rscript` or `R CMD` followed by the script path should work as well. Where possible the file paths are relative but it's possible if the code doesn't run that's the first place to look and modify to fit your local environment.

The final tuned models as determined by a grid-search using 4-fold cross-validation is in the `models` folder. Hyper-parameter tuning and learning curve plots for each model are within the `img` folder.
