# onehotcsv.R

library(tidyverse)
library(caret)

load("dat/nicu-latest.RData")

# sample up to 2e5 (20,000)
nicu.train <- nicu.train %>% sample_n(1e4) %>% select(e_comp, nicu.features$x) # cut to 10k
nicu.test <- nicu.test %>% select(e_comp, nicu.features$x) 

set.seed(1)

dummy.train <- dummyVars(~., data=nicu.train, fullRank=T)
nicu.train.hot <- data.frame(predict(dummy.train, newdata=nicu.train))
write.csv(nicu.train.hot, "dat/nicu-train-hot.csv", quote=F, row.names=F)

dummy.test  <- dummyVars(~., data=nicu.test, fullRank=T)
nicu.test.hot  <- data.frame(predict(dummy.test,  newdata=nicu.test))
write.csv(nicu.test.hot,  "dat/nicu-test-hot.csv",  quote=F, row.names=F)
