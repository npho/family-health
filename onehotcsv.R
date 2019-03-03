# onehotcsv.R

library(tidyverse)
library(caret)

load("dat/nicu-latest.RData")

# sample up to 2e5 (20,000)
nicu.train <- nicu.train %>% sample_n(1e4) %>% select(e_comp, nicu.features$x) # cut to 10k
nicu.test <- nicu.test %>% select(e_comp, nicu.features$x) 

set.seed(1)

# scale training data
scale.train <- preProcess(nicu.train, method=c("center", "scale"))
scale.train <- data.frame(predict(scale.train, newdata=nicu.train))

# one hot encode training data
dummy.train <- dummyVars(~., data=scale.train, fullRank=T)
nicu.train.hot <- data.frame(predict(dummy.train, newdata=scale.train))
write.csv(nicu.train.hot, "dat/nicu-train-hot.csv", quote=F, row.names=F)

# scale testing data
scale.test <- preProcess(nicu.test, method=c("center", "scale"))
scale.test <- data.frame(predict(scale.test, newdata=nicu.test))

# one hot encode testing data
dummy.test  <- dummyVars(~., data=scale.test, fullRank=T)
nicu.test.hot  <- data.frame(predict(dummy.test,  newdata=scale.test))
write.csv(nicu.test.hot,  "dat/nicu-test-hot.csv",  quote=F, row.names=F)
