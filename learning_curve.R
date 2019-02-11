# learning_curve.R

library(tidyverse)
library(pROC)

learning_curve <- function( f=NULL, dat=NULL, features=c(), outcome="",
                            test=0.25, prop=c(0.01, seq(0.2, 1.0, 0.2)),
                            method="", metric="ROC", control=NULL, bestTune=NULL,
                            preProcess=c("center", "scale")
                            ) 
{
    cat("[+] Generating learning curve for", method, "using", metric, "...\n")
    cat("\tValidation set is", test, "of entire data\n")
    cat("\tTraining data is", nrow(dat), "rows\n")

    i <- sample(1:nrow(dat), floor(test*nrow(dat)))
    dat.test  <- dat %>% slice( i)
    dat.train <- dat %>% slice(-i)

    lrncv <- data.frame(training_size=c(0), metric=c(0), data="", stringsAsFactors=F)
    for (p in prop) {
        j <- sample(1:nrow(dat.train), floor(p*nrow(dat.train)), replace=F)
        dat.train.sub <- dat.train %>% slice(j)
        cat("\tTraining on", paste0(round(p*100, digits=1), "% of data"), "( n =", length(j), ")\n") 
        lc.train <- train(  f,
                            dat=dat.train,
                            method=method,
                            metric=metric,
                            preProc=preProcess,
                            trControl=control,
                            tuneGrid=bestTune
                            )

        # retrain on training set for consistent metric
        lc.p <- predict(lc.train, dat.train.sub[, features], type="prob")
        lc.roc <- roc(response=dat.train.sub[, outcome], predictor=lc.p$Y)
        lrncv <- rbind(lrncv, c(length(j), as.numeric(lc.roc$auc), "Training"))
        
        # validation test set
        lc.p <- predict(lc.train, dat.test[, features], type="prob")
        lc.roc <- roc(response=dat.test[, outcome], predictor=lc.p$Y)
        lrncv <- rbind(lrncv, c(length(j), as.numeric(lc.roc$auc), "Testing"))
    }
    lrncv <- lrncv %>% filter(training_size > 0)
    lrncv$training_size <- as.numeric(lrncv$training_size)
    lrncv$metric <- as.numeric(lrncv$metric)
    return(lrncv)
}
