# kmc.R

library(tidyverse)
library(ggthemes)
library(gridExtra)
library(caret)
library(beepr)
library(amap)

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
distance.data <- data.frame(method=character(0), data=character(0), distance=character(0), metric=numeric(0), accuracy=numeric(0))
distance <- c("euclidean", "manhattan", "pearson")
for (i in 1:length(distance)) {
    cat("[+] kmc distance metric tuning", distance[i], "\n")
    nicu.train.clt <- Kmeans(select(nicu.train.hot, -e_comp.Y), 2, iter.max=30, nstart=10, method=distance[i])
    acc <- ((nicu.train.clt$cluster-1) == nicu.train.hot$e_comp.Y) %>% table %>% max / 1e4
    distance.data <- rbind( distance.data, data.frame(  method="kmc",
                                                        data="nicu", 
                                                        distance=distance[i], 
                                                        metric=sum(nicu.train.clt$withinss),
                                                        accuracy=acc
                                                    ) 
                            )

    ddat.train.clt <- Kmeans(select(ddat.train.hot, -alive.Y),  2, iter.max=30, nstart=10, method=distance[i])
    acc <- ((ddat.train.clt$cluster-1) == ddat.train.hot$alive.Y)  %>% table %>% max / 1e4
    distance.data <- rbind( distance.data, data.frame(  method="kmc",
                                                        data="ddat", 
                                                        distance=distance[i], 
                                                        metric=sum(ddat.train.clt$withinss),
                                                        accuracy=acc
                                                    ) 
                            )
}

print(distance.data) # report as table

#> print(distance.data) # report as table
#  method data  distance       metric accuracy
#1    kmc nicu euclidean 3.699626e+01   0.5164
#2    kmc ddat euclidean 2.459011e+01   0.7588
#3    kmc nicu manhattan 1.625353e+03   0.5097
#4    kmc ddat manhattan 1.045051e+03   0.7301
#5    kmc nicu   pearson 4.674201e-01   0.5174
#6    kmc ddat   pearson 5.387554e-02   0.5067
#> 

# Scree plots
scree.data <- data.frame(method=character(0), data=character(0), clusters=numeric(0), metric=numeric(0))
for (i in 1:5) {
    cat("[+] kmc cluster metric tuning", i, "clusters\n")
    nicu.train.clt <- Kmeans(select(nicu.train.hot, -e_comp.Y), i, iter.max=30, nstart=10, method="euclidean")
    scree.data <- rbind( scree.data, data.frame(method="kmc",
                                                data="nicu", 
                                                clusters=i, 
                                                metric=sum(nicu.train.clt$withinss)
                                                ) 
                        )

    ddat.train.clt <- Kmeans(select(ddat.train.hot, -alive.Y),  i, iter.max=30, nstart=10, method="euclidean")
    scree.data <- rbind( scree.data, data.frame(method="kmc",
                                                data="ddat", 
                                                clusters=i, 
                                                metric=sum(ddat.train.clt$withinss)
                                                ) 
                        )
}

nicu.train.clt <- Kmeans(select(nicu.train.hot, -e_comp.Y), 2, iter.max=30, nstart=10, method="euclidean")
ddat.train.clt <- Kmeans(select(ddat.train.hot, -alive.Y),  2, iter.max=30, nstart=10, method="euclidean")

((nicu.train.clt$cluster-1) == nicu.train.hot$e_comp.Y) %>% table %>% max / 1e4
((ddat.train.clt$cluster-1) == ddat.train.hot$alive.Y)  %>% table %>% max / 1e4

# Scree plot
scree.data$data <- as.factor(scree.data$data)
#print(scree.data)

#> print(scree.data)
#   method data clusters    metric
#1     kmc nicu        1  20.81496
#2     kmc ddat        1  48.22654
#3     kmc nicu        2  34.29234
#4     kmc ddat        2  67.16873
#5     kmc nicu        3  45.64571
#6     kmc ddat        3  60.02524
#7     kmc nicu        4  77.53528
#8     kmc ddat        4  61.13593
#9     kmc nicu        5  96.67023
#10    kmc ddat        5 155.66040
#> 

scree <- ggplot(scree.data, aes(x=clusters, y=metric, color=data)) + theme_bw() + 
            geom_line(size=1) + geom_point(size=3) +
            scale_color_discrete(name="Data Set",
                                breaks=c("nicu", "ddat"),
                                labels=c("Natality", "Mortality")) + 
            #theme(legend.title=element_blank(), legend.position=c(0.9, 0.1)) + 
            labs(title="K-Means Clustering Scree Plot",  x="Clusters", y="Variance") 

ggsave( "img/us-kmc.png", plot=scree,
        width=7, height=4, units="in", device="png", dpi="retina")
