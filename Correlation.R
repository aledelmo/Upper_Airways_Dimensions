library(GGally)

rm(list = ls(all.names = TRUE)) 
gc()

# Dataset ----

ds <- na.omit(read.csv('measurements.csv', sep=';', dec=","))
measurements = ds[,4:length(ds)]

# Correlation ----

ggpairs(measurements, title="correlogram with ggpairs()", method = c("everything", "pearson")) 

tmp <- cor(measurements)
tmp[!lower.tri(tmp)] <- 0

measurements <- measurements[,!apply(tmp,2,function(x) any(abs(x) > 0.9))]
