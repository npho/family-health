# final.R

library(tidyverse)
library(ggthemes)
library(gridExtra)
library(caret)
library(beepr)
library(pROC)

load("models/c50.RData") # c50.train.nicu AND c50.train.ddat
load("models/ada.RData") # ada.train.nicu AND ada.train.ddat
load("models/ann.RData") # ann.train.nicu AND ann.train.ddat
load("models/svm.RData") # svL.train.nicu, svR.train.nicu AND svL.train.ddat, svR.train.ddat
load("models/knn.RData") # knn.train.nicu AND knn.train.ddat

load("dat/nicu-latest.RData")
load("dat/ddat-latest.RData")

# sample up to 2e5 (20,000)
nicu.train <- nicu.train %>% sample_n(1e4) %>% select(e_comp, nicu.features$x) # cut to 10k
ddat.train <- ddat.train %>% sample_n(1e4) %>% select(alive,  ddat.features$x) # cut to 10k

set.seed(1)

# plot the ROC curve
#ggplot(knn.nicu.roc2, aes(x=spec, y=sens)) + geom_line() + scale_x_reverse()

# c50 natality
c50.nicu.pred <- predict(c50.train.nicu, nicu.test[, nicu.features$x], type="prob")
c50.nicu.roc1 <- roc(response=nicu.test$e_comp, predictor=c50.nicu.pred$Y)
c50.nicu.roc2 <- data.frame(model="c50", spec=c50.nicu.roc1$specificities, sens=c50.nicu.roc1$sensitivities)
print(c50.nicu.roc1$auc)
confusionMatrix(data=factor(ifelse(c50.nicu.pred$Y > 0.5, 'Y', 'N')), reference=nicu.test$e_comp, positive='Y')
beep()

# c50 mortality
c50.ddat.pred <- predict(c50.train.ddat, ddat.test[, ddat.features$x], type="prob")
c50.ddat.roc1 <- roc(response=ddat.test$alive, predictor=c50.ddat.pred$N)
c50.ddat.roc2 <- data.frame(model="c50", spec=c50.ddat.roc1$specificities, sens=c50.ddat.roc1$sensitivities)
print(c50.ddat.roc1$auc)
confusionMatrix(data=factor(ifelse(c50.ddat.pred$Y > 0.5, 'Y', 'N')), reference=ddat.test$alive, positive='N')
beep()



# ada natality
ada.nicu.pred <- predict(ada.train.nicu, nicu.test[, nicu.features$x], type="prob")
ada.nicu.roc1 <- roc(response=nicu.test$e_comp, predictor=ada.nicu.pred$Y)
ada.nicu.roc2 <- data.frame(model="ada", spec=ada.nicu.roc1$specificities, sens=ada.nicu.roc1$sensitivities)
print(ada.nicu.roc1$auc)
confusionMatrix(data=factor(ifelse(ada.nicu.pred$Y > 0.5, 'Y', 'N')), reference=nicu.test$e_comp, positive='Y')
beep()

# ada mortality
ada.ddat.pred <- predict(ada.train.ddat, ddat.test[, ddat.features$x], type="prob")
ada.ddat.roc1 <- roc(response=ddat.test$alive, predictor=ada.ddat.pred$N)
ada.ddat.roc2 <- data.frame(model="ada", spec=ada.ddat.roc1$specificities, sens=ada.ddat.roc1$sensitivities)
print(ada.ddat.roc1$auc)
confusionMatrix(data=factor(ifelse(ada.ddat.pred$Y > 0.5, 'Y', 'N')), reference=ddat.test$alive, positive='N')
beep()



### for ANN prediction, need to preprocess test data
# https://stats.stackexchange.com/questions/364382/do-i-have-to-preprocess-my-new-data-for-a-prediction-if-i-have-used-preprocessi
nicu.test.x <- predict(preProcess(nicu.train, method=c("center","scale")), nicu.test)
ddat.test.x <- predict(preProcess(ddat.train, method=c("center","scale")), ddat.test)

# ann natality
ann.nicu.pred <- predict(ann.train.nicu, nicu.test.x[, nicu.features$x], type="prob")
ann.nicu.roc1 <- roc(response=nicu.test.x$e_comp, predictor=ann.nicu.pred$Y)
ann.nicu.roc2 <- data.frame(model="ann", spec=ann.nicu.roc1$specificities, sens=ann.nicu.roc1$sensitivities)
print(ann.nicu.roc1$auc)
confusionMatrix(data=factor(ifelse(ann.nicu.pred$Y > 0.5, 'Y', 'N')), reference=nicu.test.x$e_comp, positive='Y')
beep()

# ann mortality
ann.ddat.pred <- predict(ann.train.ddat, ddat.test.x[, ddat.features$x], type="prob")
ann.ddat.roc1 <- roc(response=ddat.test.x$alive, predictor=ann.ddat.pred$N)
ann.ddat.roc2 <- data.frame(model="ann", spec=ann.ddat.roc1$specificities, sens=ann.ddat.roc1$sensitivities)
print(ann.ddat.roc1$auc)
confusionMatrix(data=factor(ifelse(ann.ddat.pred$Y > 0.5, 'Y', 'N')), reference=ddat.test.x$alive, positive='N')
beep()



# svL natality
svL.nicu.pred <- predict(svL.train.nicu, nicu.test[, nicu.features$x], type="prob")
svL.nicu.roc1 <- roc(response=nicu.test$e_comp, predictor=svL.nicu.pred$Y)
svL.nicu.roc2 <- data.frame(model="svL", spec=svL.nicu.roc1$specificities, sens=svL.nicu.roc1$sensitivities)
print(svL.nicu.roc1$auc)
confusionMatrix(data=factor(ifelse(svL.nicu.pred$Y > 0.5, 'Y', 'N')), reference=nicu.test$e_comp, positive='Y')
beep()

# svL mortality
svL.ddat.pred <- predict(svL.train.ddat, ddat.test[, ddat.features$x], type="prob")
svL.ddat.roc1 <- roc(response=ddat.test$alive, predictor=svL.ddat.pred$N)
svL.ddat.roc2 <- data.frame(model="svL", spec=svL.ddat.roc1$specificities, sens=svL.ddat.roc1$sensitivities)
print(svL.ddat.roc1$auc)
confusionMatrix(data=factor(ifelse(svL.ddat.pred$Y > 0.5, 'Y', 'N')), reference=ddat.test$alive, positive='N')
beep()



# svR natality
svR.nicu.pred <- predict(svR.train.nicu, nicu.test[, nicu.features$x], type="prob")
svR.nicu.roc1 <- roc(response=nicu.test$e_comp, predictor=svR.nicu.pred$Y)
svR.nicu.roc2 <- data.frame(model="svR", spec=svR.nicu.roc1$specificities, sens=svR.nicu.roc1$sensitivities)
print(svR.nicu.roc1$auc)
confusionMatrix(data=factor(ifelse(svR.nicu.pred$Y > 0.5, 'Y', 'N')), reference=nicu.test$e_comp, positive='Y')
beep()

# svR mortality
svR.ddat.pred <- predict(svR.train.ddat, ddat.test[, ddat.features$x], type="prob")
svR.ddat.roc1 <- roc(response=ddat.test$alive, predictor=svR.ddat.pred$N)
svR.ddat.roc2 <- data.frame(model="svR", spec=svR.ddat.roc1$specificities, sens=svR.ddat.roc1$sensitivities)
print(svR.ddat.roc1$auc)
confusionMatrix(data=factor(ifelse(svR.ddat.pred$Y > 0.5, 'Y', 'N')), reference=ddat.test$alive, positive='N')
beep()



# kNN natality
knn.nicu.pred <- predict(knn.train.nicu, nicu.test[, nicu.features$x], type="prob")
knn.nicu.roc1 <- roc(response=nicu.test$e_comp, predictor=knn.nicu.pred$Y)
knn.nicu.roc2 <- data.frame(model="knn", spec=knn.nicu.roc1$specificities, sens=knn.nicu.roc1$sensitivities)
print(knn.nicu.roc1$auc)
confusionMatrix(data=factor(ifelse(knn.nicu.pred$Y > 0.5, 'Y', 'N')), reference=nicu.test$e_comp, positive='Y')
beep()

# kNN mortality
knn.ddat.pred <- predict(knn.train.ddat, ddat.test[, ddat.features$x], type="prob")
knn.ddat.roc1 <- roc(response=ddat.test$alive, predictor=knn.ddat.pred$N)
knn.ddat.roc2 <- data.frame(model="knn", spec=knn.ddat.roc1$specificities, sens=knn.ddat.roc1$sensitivities)
print(knn.ddat.roc1$auc)
confusionMatrix(data=factor(ifelse(knn.ddat.pred$Y > 0.5, 'Y', 'N')), reference=ddat.test$alive, positive='N')
beep()
