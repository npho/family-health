# onehotcsv.R

library(tidyverse)
library(caret)

load("dat/nicu-latest.RData")
load("dat/ddat-latest.RData")

# sample up to 2e5 (20,000)
nicu.train <- nicu.train %>% sample_n(1e4) %>% select(e_comp, nicu.features$x) # cut to 10k
nicu.test <- nicu.test %>% select(e_comp, nicu.features$x) 

ddat.train <- ddat.train %>% sample_n(1e4) %>% select(alive,  ddat.features$x) # cut to 10k
ddat.test <- ddat.test %>% select(alive, ddat.features$x) 

set.seed(1)

preProc <- function(x, file=NULL) {
    # scale data
    scale.data <- preProcess(x, method=c("center", "scale"))
    scale.data <- data.frame(predict(scale.data, newdata=x))

    # one hot encode data
    dummy.train <- dummyVars(~., data=scale.data, fullRank=T)
    data.hot <- data.frame(predict(dummy.train, newdata=scale.data))
    if (!is.null(file)) {
        write.csv(data.hot, file, quote=F, row.names=F)
    }
    
    return(data.hot)
}

nicu.train.hot <- preProc(nicu.train)
nicu.test.hot  <- preProc(nicu.test)
ddat.train.hot <- preProc(ddat.train)
ddat.test.hot  <- preProc(ddat.test)
