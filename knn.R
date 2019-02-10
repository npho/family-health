# knn.R

library(tidyverse)
library(ggthemes)
library(gridExtra)
library(caret)
library(beepr)
library(kknn)

library(parallel)
library(doMC)
registerDoMC(cores=detectCores())

load("dat/nicu-latest.RData")
load("dat/ddat-latest.RData")

# sample up to 2e5 (20,000)
nicu.train <- nicu.train %>% sample_n(1e4) %>% select(e_comp, nicu.features$x) # cut to 10k
ddat.train <- ddat.train %>% sample_n(1e4) %>% select(alive,  ddat.features$x) # cut to 10k

set.seed(1)

knn.ctrl <- trainControl(method="cv", number=4, classProbs=T, summaryFunction=twoClassSummary, savePredictions=T)
knn.tune <- expand.grid(kmax=c(1, 3, 5, 7, 9, 11), distance=c(2), kernel=c("rectangular", "inv"))



cat("[+] knn natality data parameter tuning...")
start.run <- Sys.time()
knn.train.nicu <- train(e_comp ~ .,
                        dat=nicu.train,
                        method="kknn",
                        metric="ROC",
                        trControl=knn.ctrl,
                        tuneGrid=knn.tune
                        )
end.run <- Sys.time()
cat("done!\n")
cat("\tBest model:", paste(knn.train.nicu$bestTune %>% colnames, "=", knn.train.nicu$bestTune), "\n")
cat("\tRun time:", difftime(end.run, start.run, units="mins"), "minutes\n")
beep()

cat("[+] knn natality data learning curve...\n")
start.run <- Sys.time()
knn.lrncv.nicu <- learing_curve_dat(dat=nicu.train,
                                    outcome="e_comp",
                                    proportion=c(0.01, seq(0.2, 1.0, 0.2)),
                                    test_prop=1/4,
                                    method="kknn",
                                    metric="ROC",
                                    trControl=knn.ctrl,
                                    tuneGrid=knn.train.nicu$bestTune
                                  )
end.run <- Sys.time()
cat("    ...done!\n")
cat("\tRun time:", difftime(end.run, start.run, units="mins"), "minutes\n\n")
beep()



cat("[+] knn mortality data parameter tuning...")
start.run <- Sys.time()
knn.train.ddat <- train(alive ~ .,
                        dat=ddat.train,
                        method="kknn",
                        metric="ROC",
                        trControl=knn.ctrl,
                        tuneGrid=knn.tune
                        )
end.run <- Sys.time()
cat("done!\n")
cat("\tBest model:", paste(knn.train.ddat$bestTune %>% colnames, "=", knn.train.ddat$bestTune), "\n")
cat("\tRun time:", difftime(end.run, start.run, units="mins"), "minutes\n")
beep()

cat("[+] knn mortality data learning curve...\n")
start.run <- Sys.time()
knn.lrncv.ddat <- learing_curve_dat(dat=ddat.train,
                                    outcome="alive",
                                    proportion=c(0.01, seq(0.2, 1.0, 0.2)),
                                    test_prop=1/4,
                                    method="kknn",
                                    metric="ROC",
                                    trControl=knn.ctrl,
                                    tuneGrid=knn.train.ddat$bestTune
                                    )
end.run <- Sys.time()
cat("    ...done!\n")
cat("\tRun time:", difftime(end.run, start.run, units="mins"), "minutes\n\n")
beep()



cat("[+] Saving knn models to disk...")
save(knn.train.nicu, knn.train.ddat, file=paste0("~/src/family-health/models/knn.RData"))
cat("done!\n\n")
beep()



cat("[+] Creating knn plots...")
# natality plots
p1 <- ggplot(knn.train.nicu$results, aes(x=kmax, y=ROC, color=kernel)) + 
geom_line(size=1) + geom_point(size=3) +
labs(x="k", y="ROC (AUC)", subtitle="a.) Natality kNN Parameter Tuning") + 
theme_bw() + theme(legend.title=element_blank(), legend.position=c(0.8, 0.2))

p2 <- ggplot(knn.lrncv.nicu %>% filter(Data!="Resampling"), aes(x=Training_Size/max(Training_Size), y=1-ROC, color=Data)) + 
geom_line(size=1) + geom_point(size=3) + xlim(0.0, 1.0) +
labs(x="Portion of Data", y="ROC (1-AUC)", subtitle="b.) Natality kNN Learning Curve") + 
theme_bw() + theme(legend.title=element_blank(), legend.position=c(0.8, 0.8))

# mortality plots
p3 <- ggplot(knn.train.ddat$results, aes(x=kmax, y=ROC, color=kernel)) +
geom_line(size=1) + geom_point(size=3) +
labs(x="k", y="ROC (AUC)", subtitle="c.) Mortality kNN Parameter Tuning") + 
theme_bw() + theme(legend.title=element_blank(), legend.position=c(0.8, 0.2))

p4 <- ggplot(knn.lrncv.ddat %>% filter(Data!="Resampling"), aes(x=Training_Size/max(Training_Size), y=1-ROC, color=Data)) + 
geom_line(size=1) + geom_point(size=3) + xlim(0.0, 1.0) +
labs(x="Portion of Data", y="ROC (1-AUC)", subtitle="d.) Mortality kNN Learning Curve") + 
theme_bw() + theme(legend.title=element_blank(), legend.position=c(0.8, 0.8))

# final plot
ggsave( "img/knn.png", plot=grid.arrange(p1, p2, p3, p4, ncol=2), 
        width=8, height=5, units="in", device="png", dpi="retina")
cat("done!\n")
beep()
