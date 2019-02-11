# debug.R scratch pad for code

dt.treex <- list()
dt.rulex <- list()

dt.treextest <- list()
dt.rulextest <- list()

start <- Sys.time()

for (i in c(1, seq(2,10,2), seq(20,50,10))) {
cat(rep("-", 10), "\n")
cat("[+] Training ( trial =", i, ")\n")

cat("\ttree (")
ctrl <- C5.0Control(subset=FALSE, # group categorical vars (or not) doesn't help w/small categories (default TRUE)
                    noGlobalPruning=FALSE, # prune tree before end (or not) (default FALSE)
                    winnow=FALSE, # feature selection, new for C5.0 vs C4.5 (default FALSE)
                    sample=0.0, # use all samples to train data (defult 0.0)
                    CF=0.25, # increase CF means increase tree complexity (default 0.25)
                    minCases=1e1, # min no. of samples put into e/split, ctrls tree depth (default 2)
                    earlyStopping=FALSE, # stops multi-trials early (default TRUE)
                    fuzzyThreshold=FALSE, # how to treat continuous variables (default FALSE)
                    #seed=1, # random no. seed, manually set for reproducibility
                    ) 
start.run <- Sys.time()
dt.treex[[i]] <- C5.0(x=data.train[, nicu.features$x], y=data.train$b_comp, rules=FALSE, trials=i, control=ctrl)
end.run <- Sys.time()
cat(difftime(end.run, start.run, units="mins"), "minutes)\n")

cat("\trule (")
start.run <- Sys.time()
dt.rulex[[i]] <- C5.0(x=data.train[, nicu.features$x], y=data.train$b_comp, rules=TRUE, trials=i, control=ctrl)
end.run <- Sys.time()
cat(difftime(end.run, start.run, units="mins"), "minutes)\n")

cat("[+] Inference\n")

cat("\ttree (")
start.run <- Sys.time()
dt.treextest[[i]] <- table(predict(dt.treex[[i]], newdata=data.test[, nicu.features$x]) == data.test$b_comp)
end.run <- Sys.time()
cat(difftime(end.run, start.run, units="mins"), "minutes)\n")

cat("\trule (")
start.run <- Sys.time()
dt.rulextest[[i]] <- table(predict(dt.rulex[[i]], newdata=data.test[, nicu.features$x]) == data.test$b_comp)
end.run <- Sys.time()
cat(difftime(end.run, start.run, units="mins"), "minutes)\n")
}
end <- Sys.time()
cat(rep("-", 10), "\n")
cat("Experiment took", difftime(end, start, units="mins"), "minutes.\n")

#dt.treextest %>% prop.table
#dt.rulextest %>% prop.table
