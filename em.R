# em.R

library(tidyverse)
library(ggthemes)
library(gridExtra)
library(caret)
library(beepr)
library(mclust)

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

set.seed(1)

# Distance functions
#model.data <- data.frame(data=character(0), model=character(0), metric=numeric(0), accuracy=numeric(0))

cat("[+] EM distance metric tuning \n")
#models <- c("VII", "VVI", "VVV") # https://www.stat.washington.edu/sites/default/files/files/reports/2012/tr597.pdf
nicu.bic <- mclustBIC(select(nicu.train.hot, -e_comp.Y))
nicu.train.clt <- Mclust(select(nicu.train.hot, -e_comp.Y), x=nicu.bic, G=2)
acc <- ((nicu.train.clt$classification-1) == nicu.train.hot$e_comp.Y) %>% table %>% max / 1e4
cat("    - NICU accuracy for G=2 is", acc, "\n")

ddat.bic <- mclustBIC(select(ddat.train.hot, -alive.Y))
ddat.train.clt <- Mclust(select(ddat.train.hot, -alive.Y), x=ddat.bic, G=2)
acc <- ((ddat.train.clt$classification-1) == nicu.train.hot$e_comp.Y) %>% table %>% max / 1e4
cat("    - DDAT accuracy for G=2 is", acc, "\n")

cat("[+] EM distance saving plot to disk \n")
png("img/us-emc.png", width=10, height=5, units="in", res=300)
par(mfrow=c(1,2)) # 1 row, 2 col
plot(nicu.bic, xlab="Clusters", ylab="BIC (Natality Data)")
plot(ddat.bic, xlab="Clusters", ylab="BIC (Mortality Data)")
dev.off()
