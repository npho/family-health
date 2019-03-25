# dimreduction.R

library(tidyverse)
library(ggthemes)
library(gridExtra)
library(caret)
library(beepr)

library(parallel)
library(doMC)
registerDoMC(cores=detectCores())

source("onehot.R")

### nicu data
# nicu.train.hot
# nicu.test.hot 

### ddat data
# ddat.train.hot
# ddat.test.hot

### PCA
set.seed(1)
library(mclust)

nicu.pca <- prcomp(select(nicu.train.hot, -e_comp.Y))
ddat.pca <- prcomp(select(ddat.train.hot, -alive.Y))

nicu.pca.viz <- cbind(nicu.pca$x[,1], nicu.pca$x[,2], nicu.train.hot$e_comp.Y) %>% as.data.frame
ddat.pca.viz <- cbind(ddat.pca$x[,1], ddat.pca$x[,2], ddat.train.hot$alive.Y)  %>% as.data.frame

colnames(nicu.pca.viz) <- colnames(ddat.pca.viz) <- c("PC1", "PC2", "Class")

nicu.pca.viz$Class <- as.factor(nicu.pca.viz$Class)
ddat.pca.viz$Class <- as.factor(ddat.pca.viz$Class)

p1 <- ggplot(nicu.pca.viz, aes(x=PC1, y=PC2, color=Class)) + geom_point() + geom_density2d() +
        labs(title="a.) PCA Natality Data",  x="PC1", y="PC2") + theme_linedraw()
p2 <- ggplot(ddat.pca.viz, aes(x=PC1, y=PC2, color=Class)) + geom_point() + geom_density2d() +
        labs(title="b.) PCA Mortality Data", x="PC1", y="PC2") + theme_linedraw()

#grid.arrange(p1, p2, nrow=1)

ggsave( "img/us-pca.png", plot=grid.arrange(p1, p2, nrow=1),
        width=10, height=5, units="in", device="png", dpi="retina")



### ICA
set.seed(1)
library(fastICA)

nicu.ica <- fastICA(select(nicu.train.hot, -e_comp.Y), n.comp=2)
ddat.ica <- fastICA(select(ddat.train.hot, -alive.Y),  n.comp=2)

nicu.ica.viz <- cbind(nicu.ica$S, nicu.train.hot$e_comp.Y) %>% as.data.frame
ddat.ica.viz <- cbind(ddat.ica$S, ddat.train.hot$alive.Y)  %>% as.data.frame

colnames(nicu.ica.viz) <- colnames(ddat.ica.viz) <- c("PC1", "PC2", "Class")

nicu.ica.viz$Class <- as.factor(nicu.ica.viz$Class)
ddat.ica.viz$Class <- as.factor(ddat.ica.viz$Class)

p3 <- ggplot(nicu.ica.viz, aes(x=PC1, y=PC2, color=Class)) + geom_point() + geom_density2d() +
        labs(title="a.) ICA Natality Data",  x="PC1", y="PC2") + theme_linedraw()
p4 <- ggplot(ddat.ica.viz, aes(x=PC1, y=PC2, color=Class)) + geom_point() + geom_density2d() +
        labs(title="b.) ICA Mortality Data", x="PC1", y="PC2") + theme_linedraw()

#grid.arrange(p3, p4, nrow=1)

ggsave( "img/us-ica.png", plot=grid.arrange(p3, p4, nrow=1),
        width=10, height=5, units="in", device="png", dpi="retina")



### Randomized Projection
set.seed(1)
library(RandPro)

nicu.rmat <- form_matrix(ncol(nicu.train.hot)-1, 2, FALSE)
nicu.rpj.viz <- (select(nicu.train.hot, -e_comp.Y) %>% as.matrix) %*% nicu.rmat

ddat.rmat <- form_matrix(ncol(ddat.train.hot)-1, 2, FALSE)
ddat.rpj.viz <- (select(ddat.train.hot, -alive.Y) %>% as.matrix)  %*% ddat.rmat

nicu.rpj.viz <- cbind(nicu.rpj.viz, nicu.train.hot$e_comp.Y) %>% as.data.frame
ddat.rpj.viz <- cbind(ddat.rpj.viz, ddat.train.hot$alive.Y)  %>% as.data.frame

colnames(nicu.rpj.viz) <- colnames(ddat.rpj.viz) <- c("PC1", "PC2", "Class")

nicu.rpj.viz$Class <- as.factor(nicu.rpj.viz$Class)
ddat.rpj.viz$Class <- as.factor(ddat.rpj.viz$Class)

p5 <- ggplot(nicu.rpj.viz, aes(x=PC1, y=PC2, color=Class)) + geom_point() + geom_density2d() +
        labs(title="a.) Randomized Projection Natality Data",  x="PC1", y="PC2") + theme_linedraw()
p6 <- ggplot(ddat.rpj.viz, aes(x=PC1, y=PC2, color=Class)) + geom_point() + geom_density2d() +
        labs(title="b.) Randomized Projection Mortality Data", x="PC1", y="PC2") + theme_linedraw()

#grid.arrange(p5, p6, nrow=1)

ggsave( "img/us-rpj.png", plot=grid.arrange(p5, p6, nrow=1),
        width=10, height=5, units="in", device="png", dpi="retina")



### t-SNE
set.seed(1)
library(Rtsne)

nicu.tse.viz <- Rtsne(select(nicu.train.hot, -e_comp.Y), dims=2, num_threads=0, check_duplicates=FALSE)
ddat.tse.viz <- Rtsne(select(ddat.train.hot, -alive.Y),  dims=2, num_threads=0, check_duplicates=FALSE)

nicu.tse.viz <- cbind(nicu.tse.viz$Y, nicu.train.hot$e_comp.Y) %>% as.data.frame
ddat.tse.viz <- cbind(ddat.tse.viz$Y, ddat.train.hot$alive.Y)  %>% as.data.frame

colnames(nicu.tse.viz) <- colnames(ddat.tse.viz) <- c("PC1", "PC2", "Class")

nicu.tse.viz$Class <- as.factor(nicu.tse.viz$Class)
ddat.tse.viz$Class <- as.factor(ddat.tse.viz$Class)

p7 <- ggplot(nicu.tse.viz, aes(x=PC1, y=PC2, color=Class)) + geom_point() + geom_density2d() +
        labs(title="a.) t-SNE Natality Data",  x="PC1", y="PC2") + theme_linedraw()
p8 <- ggplot(ddat.tse.viz, aes(x=PC1, y=PC2, color=Class)) + geom_point() + geom_density2d() +
        labs(title="b.) t-SNE Mortality Data", x="PC1", y="PC2") + theme_linedraw()

#grid.arrange(p7, p8, nrow=1)

ggsave( "img/us-tse.png", plot=grid.arrange(p7, p8, nrow=1),
        width=10, height=5, units="in", device="png", dpi="retina")



### ANN
ann.ctrl <- trainControl(method="cv", number=4, classProbs=T, summaryFunction=twoClassSummary, savePredictions=T)

nicu.ann.tune <- expand.grid(layer1=c(68), layer2=c(136), layer3=c(68),
                                learning.rate=c(0.1),  momentum=0.9,
                                dropout=c(0.5), activation=c("sigmoid"))
ddat.ann.tune <- expand.grid(layer1=c(85), layer2=c(255), layer3=c(85),
                                learning.rate=c(0.06), momentum=0.9,
                                dropout=c(0.5), activation=c("sigmoid"))

cat("[+] ann nicu testing w/NO feature selection...")
start.run <- Sys.time()
ann.train.nicu <- train(e_comp ~ .,
                        dat=nicu.train,
                        method="mxnet",
                        metric="ROC",
                        preProc=c("center", "scale"),
                        trControl=ann.ctrl,
                        tuneGrid=nicu.ann.tune
                        )
end.run <- Sys.time()
cat("done!\n")
cat("\tNICU control:", ann.train.nicu$results$ROC, "AUC,", difftime(end.run, start.run, units="sec"), "seconds\n")
beep()

### ANN-control

cat("[+] ann ddat testing w/NO feature selection...")
start.run <- Sys.time()
ann.train.ddat <- train(alive ~ .,
                        dat=ddat.train,
                        method="mxnet",
                        metric="ROC",
                        preProc=c("center", "scale"),
                        trControl=ann.ctrl,
                        tuneGrid=ddat.ann.tune
                        )
end.run <- Sys.time()
cat("done!\n")
cat("\tDDAT control:", ann.train.ddat$results$ROC, "AUC,", difftime(end.run, start.run, units="sec"), "seconds\n")
beep()

### ANN-PCA

nicu.pca.viz$Class <- factor(as.character(nicu.pca.viz$Class), levels=c("0", "1"), labels=c("normal", "icu"))
ddat.pca.viz$Class <- factor(as.character(ddat.pca.viz$Class), levels=c("0", "1"), labels=c("death", "alive"))

cat("[+] ann nicu testing w/PCA feature selection...")
start.run <- Sys.time()
ann.train.nicu <- train(Class ~ .,
                        dat=nicu.pca.viz,
                        method="mxnet",
                        metric="ROC",
                        preProc=c("center", "scale"),
                        trControl=ann.ctrl,
                        tuneGrid=nicu.ann.tune
                        )
end.run <- Sys.time()
cat("done!\n")
cat("\tNICU PCA:", ann.train.nicu$results$ROC, "AUC,", difftime(end.run, start.run, units="sec"), "seconds\n")
beep()

cat("[+] ann ddat testing w/PCA feature selection...")
start.run <- Sys.time()
ann.train.ddat <- train(Clss ~ .,
                        dat=ddat.pca.viz,
                        method="mxnet",
                        metric="ROC",
                        preProc=c("center", "scale"),
                        trControl=ann.ctrl,
                        tuneGrid=ddat.ann.tune
                        )
end.run <- Sys.time()
cat("done!\n")
cat("\tDDAT PCA:", ann.train.ddat$results$ROC, "AUC,", difftime(end.run, start.run, units="sec"), "seconds\n")
beep()

### ANN-ICA

nicu.ica.viz$Class <- factor(as.character(nicu.ica.viz$Class), levels=c("0", "1"), labels=c("normal", "icu"))
ddat.ica.viz$Class <- factor(as.character(ddat.ica.viz$Class), levels=c("0", "1"), labels=c("death", "alive"))

cat("[+] ann nicu testing w/ICA feature selection...")
start.run <- Sys.time()
ann.train.nicu <- train(Class ~ .,
                        dat=nicu.ica.viz,
                        method="mxnet",
                        metric="ROC",
                        preProc=c("center", "scale"),
                        trControl=ann.ctrl,
                        tuneGrid=nicu.ann.tune
                        )
end.run <- Sys.time()
cat("done!\n")
cat("\tNICU ICA:", ann.train.nicu$results$ROC, "AUC,", difftime(end.run, start.run, units="sec"), "seconds\n")
beep()

cat("[+] ann ddat testing w/ICA feature selection...")
start.run <- Sys.time()
ann.train.ddat <- train(Clss ~ .,
                        dat=ddat.ica.viz,
                        method="mxnet",
                        metric="ROC",
                        preProc=c("center", "scale"),
                        trControl=ann.ctrl,
                        tuneGrid=ddat.ann.tune
                        )
end.run <- Sys.time()
cat("done!\n")
cat("\tDDAT ICA:", ann.train.ddat$results$ROC, "AUC,", difftime(end.run, start.run, units="sec"), "seconds\n")
beep()

### ANN-RPJ

nicu.rpj.viz$Class <- factor(as.character(nicu.rpj.viz$Class), levels=c("0", "1"), labels=c("normal", "icu"))
ddat.rpj.viz$Class <- factor(as.character(ddat.rpj.viz$Class), levels=c("0", "1"), labels=c("death", "alive"))

cat("[+] ann nicu testing w/RandPro feature selection...")
start.run <- Sys.time()
ann.train.nicu <- train(Class ~ .,
                        dat=nicu.rpj.viz,
                        method="mxnet",
                        metric="ROC",
                        preProc=c("center", "scale"),
                        trControl=ann.ctrl,
                        tuneGrid=nicu.ann.tune
                        )
end.run <- Sys.time()
cat("done!\n")
cat("\tNICU RandPro:", ann.train.nicu$results$ROC, "AUC,", difftime(end.run, start.run, units="sec"), "seconds\n")
beep()

cat("[+] ann ddat testing w/RandPro feature selection...")
start.run <- Sys.time()
ann.train.ddat <- train(Clss ~ .,
                        dat=ddat.rpj.viz,
                        method="mxnet",
                        metric="ROC",
                        preProc=c("center", "scale"),
                        trControl=ann.ctrl,
                        tuneGrid=ddat.ann.tune
                        )
end.run <- Sys.time()
cat("done!\n")
cat("\tDDAT RandPro:", ann.train.ddat$results$ROC, "AUC,", difftime(end.run, start.run, units="sec"), "seconds\n")
beep()

### ANN-TSE

nicu.tse.viz$Class <- factor(as.character(nicu.tse.viz$Class), levels=c("0", "1"), labels=c("normal", "icu"))
ddat.tse.viz$Class <- factor(as.character(ddat.tse.viz$Class), levels=c("0", "1"), labels=c("death", "alive"))

cat("[+] ann nicu testing w/t-SNE feature selection...")
start.run <- Sys.time()
ann.train.nicu <- train(Class ~ .,
                        dat=nicu.tse.viz,
                        method="mxnet",
                        metric="ROC",
                        preProc=c("center", "scale"),
                        trControl=ann.ctrl,
                        tuneGrid=nicu.ann.tune
                        )
end.run <- Sys.time()
cat("done!\n")
cat("\tNICU t-SNE:", ann.train.nicu$results$ROC, "AUC,", difftime(end.run, start.run, units="sec"), "seconds\n")
beep()

cat("[+] ann ddat testing w/t-SNE feature selection...")
start.run <- Sys.time()
ann.train.ddat <- train(Clss ~ .,
                        dat=ddat.tse.viz,
                        method="mxnet",
                        metric="ROC",
                        preProc=c("center", "scale"),
                        trControl=ann.ctrl,
                        tuneGrid=ddat.ann.tune
                        )
end.run <- Sys.time()
cat("done!\n")
cat("\tDDAT t-SNE:", ann.train.ddat$results$ROC, "AUC,", difftime(end.run, start.run, units="sec"), "seconds\n")
beep()