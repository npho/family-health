# boosting.R

library(tidyverse)
library(ggthemes)
library(gridExtra)
library(caret)
library(beepr)
library(C50)
source("c50.R") # supporting C50 functions 

library(parallel)
library(doMC)
registerDoMC(cores=detectCores())

load("dat/nicu-latest.RData")
load("dat/ddat-latest.RData")

# sample up to 2e5 (20,000)
nicu.train <- nicu.train %>% sample_n(1e4) %>% select(e_comp, nicu.features$x) # cut to 10k
ddat.train <- ddat.train %>% sample_n(1e4) %>% select(alive,  ddat.features$x) # cut to 10k

set.seed(1)

ada.ctrl <- trainControl(method="cv", number=4, classProbs=TRUE, summaryFunction=twoClassSummary)
ada.tune <- expand.grid(.trials=c(3,5,10,25,50,100), .model=c("tree"), .winnow=c(FALSE), .splits=c(50,100,150,300))



cat("[+] ada natality data parameter tuning...")
start.run <- Sys.time()
ada.train.nicu <- train(e_comp ~ .,
                        dat=nicu.train,
                        method=GetC5Info(), #"C5.0",
                        metric="ROC",
                        trControl=ada.ctrl,
                        tuneGrid=ada.tune
                        )
end.run <- Sys.time()
cat("done!\n")
cat("\tBest model:", paste(ada.train.nicu$bestTune %>% colnames, "=", ada.train.nicu$bestTune), "\n")
cat("\tRun time:", difftime(end.run, start.run, units="mins"), "minutes\n")
beep()

cat("[+] ada natality data learning curve...\n")
start.run <- Sys.time()
ada.lrncv.nicu <- learing_curve_dat(dat=nicu.train,
                                    outcome="e_comp",
                                    proportion=c(0.01, seq(0.2, 1.0, 0.2)),
                                    test_prop=1/4,
                                    method=GetC5Info(), #"C5.0",
                                    metric="ROC",
                                    trControl=ada.ctrl,
                                    tuneGrid=ada.train.nicu$bestTune
                                    )
end.run <- Sys.time()
cat("    ...done!\n")
cat("\tRun time:", difftime(end.run, start.run, units="mins"), "minutes\n\n")
beep()



cat("[+] ada mortality data parameter tuning...")
start.run <- Sys.time()
ada.train.ddat <- train(alive ~ .,
                        dat=ddat.train,
                        method=GetC5Info(), #"C5.0",
                        metric="ROC",
                        trControl=ada.ctrl,
                        tuneGrid=ada.tune
                        )
end.run <- Sys.time()
cat("done!\n")
cat("\tBest model:", paste(ada.train.ddat$bestTune %>% colnames, "=", ada.train.ddat$bestTune), "\n")
cat("\tRun time:", difftime(end.run, start.run, units="mins"), "minutes\n")
beep()

cat("[+] ada mortality data learning curve...\n")
start.run <- Sys.time()
ada.lrncv.ddat <- learing_curve_dat(dat=ddat.train,
                                    outcome="alive",
                                    proportion=c(0.01, seq(0.2, 1.0, 0.2)),
                                    test_prop=1/4,
                                    method=GetC5Info(), #"C5.0",
                                    metric="ROC",
                                    trControl=ada.ctrl,
                                    tuneGrid=ada.train.ddat$bestTune
                                    )
end.run <- Sys.time()
cat("    ...done!\n")
cat("\tRun time:", difftime(end.run, start.run, units="mins"), "minutes\n\n")
beep()



cat("[+] Saving ada models to disk...")
save(ada.train.nicu, ada.train.ddat, file=paste0("~/src/family-health/models/ada.RData"))
cat("done!\n\n")
beep()



cat("[+] Creating ada plots...")
# natality plots
p1 <- ggplot(ada.train.nicu$results, aes(x=trials, y=ROC, color=factor(splits))) + 
geom_line(size=1) + geom_point(size=3) +
labs(x="Iterations", y="ROC (AUC)", subtitle="a.) Natality AdaBoost Parameter Tuning") + 
theme_bw() + theme(legend.title=element_blank(), legend.position=c(0.8, 0.4))

p2 <- ggplot(ada.lrncv.nicu %>% filter(Data!="Resampling"), aes(x=Training_Size/max(Training_Size), y=1-ROC, color=Data)) + 
geom_line(size=1) + geom_point(size=3) + xlim(0.0, 1.0) +
labs(x="Portion of Data", y="ROC (1-AUC)", subtitle="b.) Natality AdaBoost Learning Curve") + 
theme_bw() + theme(legend.title=element_blank(), legend.position=c(0.8, 0.2))

# mortality plots
p3 <- ggplot(ada.train.ddat$results, aes(x=trials, y=ROC, color=factor(splits))) +
geom_line(size=1) + geom_point(size=3) +
labs(x="Iterations", y="ROC (AUC)", subtitle="c.) Mortality AdaBoost Parameter Tuning") + 
theme_bw() + theme(legend.title=element_blank(), legend.position=c(0.8, 0.4))

p4 <- ggplot(ada.lrncv.ddat %>% filter(Data!="Resampling"), aes(x=Training_Size/max(Training_Size), y=1-ROC, color=Data)) + 
geom_line(size=1) + geom_point(size=3) + xlim(0.0, 1.0) +
labs(x="Portion of Data", y="ROC (1-AUC)", subtitle="d.) Mortality AdaBoost Learning Curve") + 
theme_bw() + theme(legend.title=element_blank(), legend.position=c(0.8, 0.8))

# final plot
ggsave( "img/ada.png", plot=grid.arrange(p1, p2, p3, p4, ncol=2), 
        width=8, height=5, units="in", device="png", dpi="retina")
cat("done!\n")
beep()
