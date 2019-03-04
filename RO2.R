# RO2.R

library(tidyverse)
library(ggthemes)
library(gridExtra)

# plot count ones 
co.ga <- read.csv("ABAGAIL/countonesGA.out", header=F) %>% drop_na
co.ga$algo <- "GA"
co.mim <- read.csv("ABAGAIL/countonesMimic.out", header=F) %>% drop_na
co.mim$algo <- "MIMIC"
co.rhc <- read.csv("ABAGAIL/countonesRHC.out", header=F) %>% drop_na
co.rhc$algo <- "RHC"
co.sa <- read.csv("ABAGAIL/countonesSA.out", header=F) %>% drop_na
co.sa$algo <- "SA"
co.all <- rbind(co.ga, co.mim, co.rhc, co.sa)

p1 <- ggplot(co.all %>% filter(V1 < 500), aes(x=V1, y=V2, color=algo)) + geom_line() + 
labs(title="c.) Count Ones", x="Iterations", y="Score") + 
theme_linedraw() + theme(legend.position="bottom") + theme(legend.title=element_blank()) 

# plot flip flop  
ff.ga <- read.csv("ABAGAIL/ffGA.out", header=F) %>% drop_na
ff.ga$algo <- "GA"
ff.mim <- read.csv("ABAGAIL/ffMimic.out", header=F) %>% drop_na
ff.mim$algo <- "MIMIC"
ff.rhc <- read.csv("ABAGAIL/ffRHC.out", header=F) %>% drop_na
ff.rhc$algo <- "RHC"
ff.sa <- read.csv("ABAGAIL/ffSA.out", header=F) %>% drop_na
ff.sa$algo <- "SA"
ff.all <- rbind(ff.ga, ff.mim, ff.rhc, ff.sa)
#ff.all$V2 <- 1/ff.all$V2

p2 <- ggplot(ff.all %>% filter(V1 < 900), aes(x=V1, y=V2, color=algo)) + geom_line() + 
labs(title="b.) Flip Flop", x="Iterations", y="Score") + 
theme_linedraw() + theme(legend.position="bottom") + theme(legend.title=element_blank()) 

# plot traveling sales 
ts.ga <- read.csv("ABAGAIL/tsGA.out", header=F) %>% drop_na
ts.ga$algo <- "GA"
ts.mim <- read.csv("ABAGAIL/tsMimic.out", header=F) %>% drop_na
ts.mim$algo <- "MIMIC"
ts.rhc <- read.csv("ABAGAIL/tsRHC.out", header=F) %>% drop_na
ts.rhc$algo <- "RHC"
ts.sa <- read.csv("ABAGAIL/tsSA.out", header=F) %>% drop_na
ts.sa$algo <- "SA"
ts.all <- rbind(ts.ga, ts.mim, ts.rhc, ts.sa)

p3 <- ggplot(ts.all %>% filter(V1 < 500), aes(x=V1, y=V2, color=algo)) + geom_line() + 
labs(title="a.) Travelling Salesman", x="Iterations", y="Score") + 
theme_linedraw() + theme(legend.position="bottom") + theme(legend.title=element_blank()) 

ggsave( "img/randomized-part2.png", plot=grid.arrange(p3, p2, p1, nrow=1), 
        width=16, height=5, units="in", device="png", dpi="retina")
