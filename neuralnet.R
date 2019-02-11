# neuralnet.R

library(tidyverse)
library(ggthemes)
library(gridExtra)
library(caret)
library(beepr)
library(mxnet)
source("learning_curve.R")

library(parallel)
library(doMC)
registerDoMC(cores=detectCores())

load("dat/nicu-latest.RData")
load("dat/ddat-latest.RData")

# sample up to 2e5 (20,000)
nicu.train <- nicu.train %>% sample_n(1e4) %>% select(e_comp, nicu.features$x) # cut to 10k
ddat.train <- ddat.train %>% sample_n(1e4) %>% select(alive,  ddat.features$x) # cut to 10k

set.seed(1)

ann.ctrl <- trainControl(method="cv", number=4, classProbs=T, summaryFunction=twoClassSummary, savePredictions=T)



f <- ncol(nicu.train)-1
ann.tune <- expand.grid(layer1=c(f),
                        layer2=c(f/2, f, f*2, f*3, f*4) %>% floor,
                        layer3=c(f),
                        learning.rate=seq(0.06, 0.22, 0.04), momentum=0.9,
                        dropout=c(0.5), activation=c("sigmoid"))
cat("[+] ann natality data parameter tuning...")
start.run <- Sys.time()
ann.train.nicu <- train(e_comp ~ .,
                        dat=nicu.train,
                        method="mxnet",
                        metric="ROC",
                        preProc=c("center", "scale"),
                        trControl=ann.ctrl,
                        tuneGrid=ann.tune
                        )
end.run <- Sys.time()
cat("done!\n")
cat("\tBest model:", paste(ann.train.nicu$bestTune %>% colnames, "=", ann.train.nicu$bestTune), "\n")
cat("\tRun time:", difftime(end.run, start.run, units="mins"), "minutes\n")
beep()

cat("[+] ann natality data learning curve...\n") 
start.run <- Sys.time()
ann.lrncv.nicu <- learning_curve(e_comp ~ ., dat=nicu.train, features=nicu.features$x, outcome="e_comp",
                                    test=0.25, prop=c(0.01, seq(0.2, 1.0, 0.2)),
                                    method="mxnet", metric="ROC", 
                                    control=ann.ctrl, bestTune=ann.train.nicu$bestTune,
                                    preProcess=c("center", "scale")
                                )
end.run <- Sys.time()
cat("    ...done!\n")
cat("\tRun time:", difftime(end.run, start.run, units="mins"), "minutes\n\n")
beep()



f <- ncol(ddat.train)-1
ann.tune <- expand.grid(layer1=c(f),
                        layer2=c(f/2, f, f*2, f*3, f*4) %>% floor,
                        layer3=c(f),
                        learning.rate=seq(0.06, 0.22, 0.04), momentum=0.9,
                        dropout=c(0.5), activation=c("sigmoid"))
cat("[+] ann mortality data parameter tuning...")
start.run <- Sys.time()
ann.train.ddat <- train(alive ~ .,
                        dat=ddat.train,
                        method="mxnet",
                        metric="ROC",
                        preProc=c("center", "scale"),
                        trControl=ann.ctrl,
                        tuneGrid=ann.tune
                        )
end.run <- Sys.time()
cat("done!\n")
cat("\tBest model:", paste(ann.train.ddat$bestTune %>% colnames, "=", ann.train.ddat$bestTune), "\n")
cat("\tRun time:", difftime(end.run, start.run, units="mins"), "minutes\n")
beep()

cat("[+] ann mortality data learning curve...\n") 
start.run <- Sys.time()
ann.lrncv.ddat <- learning_curve(alive ~ ., dat=ddat.train, features=ddat.features$x, outcome="alive",
                                    test=0.25, prop=c(0.01, seq(0.2, 1.0, 0.2)),
                                    method="mxnet", metric="ROC", 
                                    control=ann.ctrl, bestTune=ann.train.ddat$bestTune,
                                    preProcess=c("center", "scale")
                                )
end.run <- Sys.time()
cat("    ...done!\n")
cat("\tRun time:", difftime(end.run, start.run, units="mins"), "minutes\n\n")
beep()



cat("[+] Saving ann models to disk...")
save(ann.train.nicu, ann.train.ddat, file=paste0("~/src/family-health/models/ann.RData"))
cat("done!\n\n")
beep()



cat("[+] Creating ann plots...")
# natality plots
p1 <- ggplot(ann.train.nicu) + 
labs(x="Number of Layer 2 Units", y="ROC (AUC)", subtitle="a.) Natality ANN Parameter Tuning") + 
theme_bw() #+ theme(legend.position=c(0.2, 0.45))

p2 <- ggplot(ann.lrncv.nicu, aes(x=training_size/max(training_size), y=1-metric, color=data)) + 
geom_line(size=1) + geom_point(size=3) + xlim(0.0, 1.0) +
labs(x="Portion of Data", y="ROC (1-AUC)", subtitle="b.) Natality ANN Learning Curve") + 
theme_bw() + theme(legend.title=element_blank(), legend.position=c(0.8, 0.5))

# mortality plots
p3 <- ggplot(ann.train.ddat) + 
labs(x="Number of Layer 2 Units", y="ROC (AUC)", subtitle="c.) Mortality ANN Parameter Tuning") + 
theme_bw() #+ theme(legend.position=c(0.2, 0.45))

p4 <- ggplot(ann.lrncv.ddat, aes(x=training_size/max(training_size), y=1-metric, color=data)) + 
geom_line(size=1) + geom_point(size=3) + xlim(0.0, 1.0) +
labs(x="Portion of Data", y="ROC (1-AUC)", subtitle="d.) Mortality ANN Learning Curve") + 
theme_bw() + theme(legend.title=element_blank(), legend.position=c(0.8, 0.2))

# final plot
ggsave( "img/ann.png", plot=grid.arrange(p1, p2, p3, p4, ncol=2), 
        width=8, height=5, units="in", device="png", dpi="retina")
cat("done!\n")
beep()
