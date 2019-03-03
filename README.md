# family-health

by: npho3

## 1. Overview

### Supervised Learning 

Survey of supervised learning approaches to analysis of family health data from the National Vital Statistics System of the National Center for Health Statistics. See submitted paper `npho3-analysis.pdf` for full details.

### Randomized Optimization

Survey of randomized optimization methods including randomized hill climbing, simulated annealing, and genetic algorithms to determine weights in a neural network. Then contrived problems for which these algorithms will thrive. See submitted paper `npho3-analysis.pdf` for further details.

## 2. Getting Data

The data sets are (1) natality data from every recorded birth in the US in 2017 and (2) the linked mortality statistics from 2012 for every child under 1 year of age who died that year. Each can be processed with the pre-processing scripts in this repository, make sure to edit the scripts to point to the appropriate location of the original data on your local computer. The pre-processing scripts made sure the columns are of the appropriate R data structure (e.g., factor, numeric) as well as manually curating the features by removing redundant or medically irrelevant (to our hypothesis) ones. Alternatively, explore the `dat` folder within the repository for the cleaned and prepared R objects saved as `RData` files. Note: make sure `git lfs` is configured locally to handle these larger files.

## 3. Library Setup

### Supervised Learning

All standard R packages used should be able to be installed directly from CRAN without issue. The only exception is MXNet for which additional hacks are detailed below.

Used [MXNet](https://mxnet.apache.org) for artificial neural networks (ANNs), instructions for installing [here](https://mxnet.incubator.apache.org/versions/master/install/index.html). For macOS the instructions say to use `homebrew` to install OpenCV. However, by default v4 gets installed and MXNet only recognized v3 when I tested. Install `opencv@3` then symbolically link the expected OpenCV path.

```r
cran <- getOption("repos")
cran["dmlc"] <- "https://apache-mxnet.s3-accelerate.dualstack.amazonaws.com/R/CRAN/"
options(repos = cran)
install.packages("mxnet")
```

### Randomized Optimization

For the randomized optimization assignment part 1 and using differing optimization algorithms for the neural network I migrated from R to Python. Use the Docker Jupyter notebooks [container](https://github.com/jupyter/docker-stacks) which have all the necessary libraries, the `RandomizedOptimization.ipynb` file installs `mlrose` automatically.

For the randomized optimization assignment part 2 some [ABAGAIL](https://abagail.readthedocs.io) code was required, Java code requires `ant` to build. I borrowed the implementation of the Traveling Salesman Problem, Countones, and Flipflop from [@proudhuma](https://github.com/proudhuma/CS4641_Asg2) that extends ABAGAIL for these experiments. Copy the java code into the ABAGAIL `src` folder then build with `ant` as usual. Run by using one of the various commands:

## 4. Running Code

### Supervised Learning 

Each supervised learning approach lives within its own self-contained R script within the base repository. The code can be run by loaded an interactive R session then calling each script via `source()`. It wasn't explicitly tested but it's likely running it fully from the command line via `Rscript` or `R CMD` followed by the script path should work as well. Where possible the file paths are relative but it's possible if the code doesn't run that's the first place to look and modify to fit your local environment.

The final tuned models as determined by a grid-search using 4-fold cross-validation is in the `models` folder. Hyper-parameter tuning and learning curve plots for each model are within the `img` folder.

### Randomized Optimization

For part 1 in optimizing neural network weights you can run the command below to load the Docker container then open your browser and navigate to this [link](http://localhost:8888).

```
docker run -p 8888:8888 -v "$PWD":/home/jovyan/work/ jupyter/datascience-notebook
```

For part 2 you can select a test to run after compiling ABAGAIL with `ant` including the additional custom Java code.

```
java -cp ABAGAIL.jar mytest.TravelingSalesmanTest
java -cp ABAGAIL.jar mytest.FlipFlopTest
java -cp ABAGAIL.jar mytest.CountOnesTest
```

Several `*.out` files will be generated with the run data from which you can generate plots.
