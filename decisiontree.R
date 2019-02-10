# decisiontree.R

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

c50.ctrl <- trainControl(method="cv", number=4, classProbs=TRUE, summaryFunction=twoClassSummary)
c50.tune <- expand.grid(.trials=c(1), .model=c("tree", "rules"), .winnow=c(FALSE), .splits=c(2,5,10,25,50,100))



cat("[+] c50 natality data parameter tuning...")
start.run <- Sys.time()
c50.train.nicu <- train(e_comp ~ .,
                        dat=nicu.train,
                        method=GetC5Info(), #"C5.0",
                        metric="ROC",
                        trControl=c50.ctrl,
                        tuneGrid=c50.tune
                        )
end.run <- Sys.time()
cat("done!\n")
cat("\tBest model:", paste(c50.train.nicu$bestTune %>% colnames, "=", c50.train.nicu$bestTune), "\n")
cat("\tRun time:", difftime(end.run, start.run, units="mins"), "minutes\n")
beep()

cat("[+] c50 natality data learning curve...\n")
start.run <- Sys.time()
c50.lrncv.nicu <- learing_curve_dat(dat=nicu.train,
                                    outcome="e_comp",
                                    proportion=c(0.01, seq(0.2, 1.0, 0.2)),
                                    test_prop=1/4,
                                    method=GetC5Info(), #"C5.0",
                                    metric="ROC",
                                    trControl=c50.ctrl,
                                    tuneGrid=c50.train.nicu$bestTune
                                    )
end.run <- Sys.time()
cat("    ...done!\n")
cat("\tRun time:", difftime(end.run, start.run, units="mins"), "minutes\n\n")
beep()



cat("[+] c50 mortality data parameter tuning...")
start.run <- Sys.time()
c50.train.ddat <- train(alive ~ .,
                        dat=ddat.train,
                        method=GetC5Info(), #"C5.0",
                        metric="ROC",
                        trControl=c50.ctrl,
                        tuneGrid=c50.tune
                        )
end.run <- Sys.time()
cat("done!\n")
cat("\tBest model:", paste(c50.train.nicu$bestTune %>% colnames, "=", c50.train.nicu$bestTune), "\n")
cat("\tRun time:", difftime(end.run, start.run, units="mins"), "minutes\n")
beep()

cat("[+] c50 mortality data learning curve...\n")
start.run <- Sys.time()
c50.lrncv.ddat <- learing_curve_dat(dat=ddat.train,
                                    outcome="alive",
                                    proportion=c(0.01, seq(0.2, 1.0, 0.2)),
                                    test_prop=1/4,
                                    method=GetC5Info(), #"C5.0",
                                    metric="ROC",
                                    trControl=c50.ctrl,
                                    tuneGrid=c50.train.ddat$bestTune
                                    )
end.run <- Sys.time()
cat("    ...done!\n")
cat("\tRun time:", difftime(end.run, start.run, units="mins"), "minutes\n\n")
beep()



cat("[+] Saving c50 models to disk...")
save(c50.train.nicu, c50.train.ddat, file=paste0("~/src/family-health/models/c50.RData"))
cat("done!\n\n")
beep()



cat("[+] Creating c50 plots...")
# natality plots
p1 <- ggplot(c50.train.nicu$results, aes(x=splits, y=ROC, color=model)) + 
geom_line(size=1) + geom_point(size=3) +
labs(x="Splits", y="ROC (AUC)", subtitle="a.) Natality C5.0 Parameter Tuning") + 
theme_bw() + theme(legend.title=element_blank(), legend.position=c(0.8, 0.2))

p2 <- ggplot(c50.lrncv.nicu %>% filter(Data!="Resampling"), aes(x=Training_Size/max(Training_Size), y=1-ROC, color=Data)) + 
geom_line(size=1) + geom_point(size=3) + xlim(0.0, 1.0) +
labs(x="Portion of Data", y="ROC (1-AUC)", subtitle="b.) Natality C5.0 Learning Curve") + 
theme_bw() + theme(legend.title=element_blank(), legend.position=c(0.8, 0.2))

# mortality plots
p3 <- ggplot(c50.train.ddat$results, aes(x=splits, y=ROC, color=model)) +
geom_line(size=1) + geom_point(size=3) +
labs(x="Splits", y="ROC (AUC)", subtitle="c.) Mortality C5.0 Parameter Tuning") + 
theme_bw() + theme(legend.title=element_blank(), legend.position=c(0.8, 0.2))

p4 <- ggplot(c50.lrncv.ddat %>% filter(Data!="Resampling"), aes(x=Training_Size/max(Training_Size), y=1-ROC, color=Data)) + 
geom_line(size=1) + geom_point(size=3) + xlim(0.0, 1.0) +
labs(x="Portion of Data", y="ROC (1-AUC)", subtitle="d.) Mortality C5.0 Learning Curve") + 
theme_bw() + theme(legend.title=element_blank(), legend.position=c(0.8, 0.2))

# final plot
ggsave( "img/c50.png", plot=grid.arrange(p1, p2, p3, p4, ncol=2), 
        width=8, height=5, units="in", device="png", dpi="retina")
cat("done!\n")
beep()
