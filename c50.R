# c50.R

# Cite: https://www.euclidean.com/machine-learning-in-practice/2015/6/12/r-caret-and-parameter-tuning-c50

library(plyr)

C5CustomSort <- function(x) {
  x$model <- factor(as.character(x$model), levels = c("rules","tree"))
  x[order(x$trials, x$model, x$splits, !x$winnow),]
}

C5CustomLoop <- function (grid) {
    loop <- ddply(grid, c("model", "winnow","splits"), function(x) c(trials = max(x$trials)))
    submodels <- vector(mode = "list", length = nrow(loop))
    for (i in seq(along = loop$trials)) {
        index <- which(grid$model == loop$model[i] & grid$winnow == 
            loop$winnow[i] & grid$splits == loop$splits[i])
        trials <- grid[index, "trials"]
        submodels[[i]] <- data.frame(trials = trials[trials != 
            loop$trials[i]])
    }
    list(loop = loop, submodels = submodels)
}

C5CustomGrid <- function(x, y, len = NULL) {
  c5seq <- if(len == 1)  1 else  c(1, 10*((2:min(len, 11)) - 1))
  expand.grid(trials = c5seq, splits = c(2,10,20,50), winnow = c(TRUE, FALSE), model = c("tree","rules"))
}

C5CustomFit <- function(x, y, wts, param, lev, last, classProbs, ...) {
  # add the splits parameter to the fit function
  # minCases is a function of splits
  
  theDots <- list(...)

  splits   <- param$splits
  minCases <- floor( length(y)/splits ) - 1

  if (any(names(theDots) == "control")) {
    theDots$control$winnow        <- param$winnow
    theDots$control$minCases      <- minCases
    theDots$control$earlyStopping <- FALSE
  }
  else
  theDots$control <- C5.0Control(winnow = param$winnow, minCases = minCases, earlyStopping=FALSE )

  argList <- list(x = x, y = y, weights = wts, trials = param$trials, rules = param$model == "rules")

  argList <- c(argList, theDots)

  do.call("C5.0.default", argList)
}

GetC5Info <- function() {
  # get the default C5.0 model functions
  c5ModelInfo <- getModelInfo(model = "C5.0", regex = FALSE)[[1]]

  # modify the parameters data frame so that it includes splits
  c5ModelInfo$parameters$parameter <- factor(c5ModelInfo$parameters$parameter,levels=c(levels(c5ModelInfo$parameters$parameter),'splits'))
  c5ModelInfo$parameters$label <- factor(c5ModelInfo$parameters$label,levels=c(levels(c5ModelInfo$parameters$label),'Splits'))
  c5ModelInfo$parameters <- rbind(c5ModelInfo$parameters,c('splits','numeric','Splits'))
  
  # replace the default c5.0 functions with ones that are aware of the splits parameter
  c5ModelInfo$fit  <- C5CustomFit
  c5ModelInfo$loop <- C5CustomLoop
  c5ModelInfo$grid <- C5CustomGrid
  c5ModelInfo$sort <- C5CustomSort

  return (c5ModelInfo)
}
