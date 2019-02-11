# svm.R

library(tidyverse)
library(ggthemes)
library(gridExtra)
library(caret)
library(beepr)
library(kernlab)

library(parallel)
library(doMC)
registerDoMC(cores=detectCores())

load("dat/nicu-latest.RData")
load("dat/ddat-latest.RData")

# sample up to 2e5 (20,000)
nicu.train <- nicu.train %>% sample_n(1e4) %>% select(e_comp, nicu.features$x) # cut to 10k
ddat.train <- ddat.train %>% sample_n(1e4) %>% select(alive,  ddat.features$x) # cut to 10k

set.seed(1)

svm.ctrl <- trainControl(method="cv", number=4, classProbs=T, summaryFunction=twoClassSummary, savePredictions=T)
svL.tune <- expand.grid(C=c(0.01, 0.10, 0.50, 1.00, 1.50, 2.00))
svR.tune <- cbind(svL.tune, sigma=0.5)



cat("[+] svm natality data parameter tuning [linear kernel]...")
start.run <- Sys.time()
svL.train.nicu <- train(e_comp ~ .,
                        dat=nicu.train,
                        method="svmLinear",
                        metric="ROC",
                        trControl=svm.ctrl,
                        tuneGrid=svL.tune,
                        )
end.run <- Sys.time()
cat("done!\n")
cat("\tBest model:", paste(svL.train.nicu$bestTune %>% colnames, "=", svL.train.nicu$bestTune), "\n")
cat("\tRun time:", difftime(end.run, start.run, units="mins"), "minutes\n")
beep()

cat("[+] svm natality data parameter tuning [radial kernel]...")
start.run <- Sys.time()
svR.train.nicu <- train(e_comp ~ .,
                        dat=nicu.train,
                        method="svmRadial",
                        metric="ROC",
                        trControl=svm.ctrl,
                        tuneGrid=svR.tune,
                        )
end.run <- Sys.time()
cat("done!\n")
cat("\tBest model:", paste(svR.train.nicu$bestTune %>% colnames, "=", svR.train.nicu$bestTune), "\n")
cat("\tRun time:", difftime(end.run, start.run, units="mins"), "minutes\n")
beep()

cat("[+] svm mortality data parameter tuning [linear kernel]...")
start.run <- Sys.time()
svL.train.ddat <- train(alive ~ .,
                        dat=ddat.train,
                        method="svmLinear",
                        metric="ROC",
                        trControl=svm.ctrl,
                        tuneGrid=svL.tune,
                        )
end.run <- Sys.time()
cat("done!\n")
cat("\tBest model:", paste(svL.train.ddat$bestTune %>% colnames, "=", svL.train.ddat$bestTune), "\n")
cat("\tRun time:", difftime(end.run, start.run, units="mins"), "minutes\n")
beep()

cat("[+] svm mortality data parameter tuning [radial kernel]...")
start.run <- Sys.time()
svR.train.ddat <- train(alive ~ .,
                        dat=ddat.train,
                        method="svmRadial",
                        metric="ROC",
                        trControl=svm.ctrl,
                        tuneGrid=svR.tune,
                        )
end.run <- Sys.time()
cat("done!\n")
cat("\tBest model:", paste(svR.train.ddat$bestTune %>% colnames, "=", svR.train.ddat$bestTune), "\n")
cat("\tRun time:", difftime(end.run, start.run, units="mins"), "minutes\n")
beep()



cat("[+] svm natality data learning curve [linear kernel]...\n")
start.run <- Sys.time()
svL.lrncv.nicu <- learning_curve(e_comp ~ ., dat=nicu.train, features=nicu.features$x, outcome="e_comp",
                                    test=0.25, prop=c(0.01, seq(0.2, 1.0, 0.2)),
                                    method="svmLinear", metric="ROC", 
                                    control=svm.ctrl, bestTune=svL.train.nicu$bestTune,
                                    preProcess=NULL
                                )
end.run <- Sys.time()
cat("    ...done!\n")
cat("\tRun time:", difftime(end.run, start.run, units="mins"), "minutes\n\n")
beep()

cat("[+] svm natality data learning curve [radial kernel]...\n")
start.run <- Sys.time()
svR.lrncv.nicu <- learning_curve(e_comp ~ ., dat=nicu.train, features=nicu.features$x, outcome="e_comp",
                                    test=0.25, prop=c(0.01, seq(0.2, 1.0, 0.2)),
                                    method="svmRadial", metric="ROC", 
                                    control=svm.ctrl, bestTune=svR.train.nicu$bestTune,
                                    preProcess=NULL
                                )
end.run <- Sys.time()
cat("    ...done!\n")
cat("\tRun time:", difftime(end.run, start.run, units="mins"), "minutes\n\n")
beep()

cat("[+] svm mortality data learning curve [linear kernel]...\n")
start.run <- Sys.time()
svL.lrncv.ddat <- learning_curve(alive ~ ., dat=ddat.train, features=ddat.features$x, outcome="alive",
                                    test=0.25, prop=c(0.01, seq(0.2, 1.0, 0.2)),
                                    method="svmLinear", metric="ROC", 
                                    control=svm.ctrl, bestTune=svL.train.ddat$bestTune,
                                    preProcess=NULL
                                )
end.run <- Sys.time()
cat("    ...done!\n")
cat("\tRun time:", difftime(end.run, start.run, units="mins"), "minutes\n\n")
beep()

cat("[+] svm mortality data learning curve [radial kernel]...\n")
start.run <- Sys.time()
svR.lrncv.ddat <- learning_curve(alive ~ ., dat=ddat.train, features=ddat.features$x, outcome="alive",
                                    test=0.25, prop=c(0.01, seq(0.2, 1.0, 0.2)),
                                    method="svmRadial", metric="ROC", 
                                    control=svm.ctrl, bestTune=svR.train.ddat$bestTune,
                                    preProcess=NULL
                                )
end.run <- Sys.time()
cat("    ...done!\n")
cat("\tRun time:", difftime(end.run, start.run, units="mins"), "minutes\n\n")
beep()



cat("[+] Saving svm models to disk...")
save(svL.train.nicu, svR.train.nicu, svL.train.ddat, svR.train.ddat, 
        file=paste0("~/src/family-health/models/svm.RData"))
cat("done!\n\n")
beep()



cat("[+] Creating svm plots...")
# parameter tuning plots
svm.plot <- rbind(
cbind(svL.train.nicu$results[, c("C", "ROC")], kernel="linear", data="natality"),
cbind(svR.train.nicu$results[, c("C", "ROC")], kernel="radial", data="natality"),
cbind(svL.train.ddat$results[, c("C", "ROC")], kernel="linear", data="mortality"),
cbind(svR.train.ddat$results[, c("C", "ROC")], kernel="radial", data="mortality")
)
p1 <- ggplot(svm.plot) + 
geom_line(aes(C, ROC, color=kernel, group=interaction(kernel, data)), size=1) + 
geom_point(aes(C, ROC, color=kernel, shape=data, group=interaction(kernel, data)), size=3) +
scale_colour_discrete(name="Kernel", breaks=c("linear", "radial"),labels=c("linear", "radial")) +
scale_shape_discrete(name="Data", breaks=c("natality", "mortality"),labels=c("natality", "mortality")) +
labs(x="Cost", y="ROC (AUC)", subtitle="a.) SVM Parameter Tuning") + 
theme_bw() + theme(legend.position="right")

# learning curve plots
p2 <- ggplot(svL.lrncv.nicu, aes(training_size/max(training_size), 1-metric)) + 
geom_line(aes(color=data), size=1) + 
geom_point(aes(color=data, shape=data), size=3) +
geom_line(data=svL.lrncv.ddat, aes(color=data), size=1) + 
geom_point(data=svL.lrncv.ddat, aes(color=data, shape=data), size=3) +
labs(x="Portion of Data", y="ROC (1-AUC)", subtitle="b.) SVM Learning Curve (Linear Kernel)") + 
theme_bw() + theme(legend.title=element_blank(), legend.position=c(0.65, 0.3))

p3 <- ggplot(svR.lrncv.nicu, aes(training_size/max(training_size), 1-metric)) + 
geom_line(aes(color=data), size=1) + 
geom_point(aes(color=data, shape=data), size=3) +
geom_line(data=svR.lrncv.ddat, aes(color=data), size=1) + 
geom_point(data=svR.lrncv.ddat, aes(color=data, shape=data), size=3) +
labs(x="Portion of Data", y="ROC (1-AUC)", subtitle="c.) SVM Learning Curve (Radial Kernel)") + 
theme_bw() + theme(legend.title=element_blank(), legend.position=c(0.65, 0.3))

# final plot
ggsave( "img/svm.png", plot=grid.arrange(p1, p2, p3, ncol=3), 
        width=11, height=4, units="in", device="png", dpi="retina")
cat("done!\n")
beep()
