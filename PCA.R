rm(list = ls(all.names = TRUE)) 
gc()

# Dataset ----

ds <- na.omit(read.csv('measurements.csv', sep=';', dec=","))
measurements = ds[,4:length(ds)]
summary(measurements)

# PCA ----

res.pca <- prcomp(measurements, scale = TRUE)

fviz_eig(res.pca)

fviz_pca_ind(res.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = rev(brewer.pal(n = nrow(measurements), name = "Spectral")),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = rev(brewer.pal(n = nrow(measurements), name = "Spectral")),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_biplot(res.pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)

summary(res.pca)
