library(ggplot2)
library(hrbrthemes)
library(factoextra)
library("RColorBrewer")

rm(list = ls(all.names = TRUE)) 
gc()

# Dataset ----

ds <- na.omit(read.csv('measurements.csv', sep=';', dec=","))
measurements = ds[,4:length(ds)]

age = measurements[,1]
ap_glottic_diameter = measurements[,9]
ap_cricoid_diameter = measurements[,10]

# Glottic Model ----

model_glottic <- lm(ap_glottic_diameter ~ poly(age,3))
#summary(model_glottic)
#confint(model_glottic, level=0.95)

predicted_intervals <- predict(model_glottic,data.frame(x=age),interval='confidence', level=0.99)
measurements_predict = cbind(measurements, predicted_intervals)

p <- (ggplot(measurements_predict, aes(x=age.en.mois, y=antéro.post.CV)) + geom_point() + 
        geom_line(aes(y = fit), col='blue',size = 1) + geom_ribbon( aes(ymin = lwr, ymax = upr, colour = 'gray'), alpha = .15, linetype = 0)
      + theme_ipsum(grid=FALSE, axis=TRUE, axis_title_just='c') + 
        labs(x="Age (months)", y="Diameter (mm)", title="Anteroposterior Glottic Diameters", caption=paste("AIC", AIC(model_glottic))) +
        ylim(0, 25) + guides(colour=FALSE))
print(p)

ggsave('/Users/imag2/Desktop/Upper_Airways_Dimensions/results/glottic.png', p)

# Cricoid Model ----

model_cricoid <- lm(ap_cricoid_diameter ~ poly(age,3))
#summary(model_cricoid)
#confint(model_cricoid, level=0.95)

predicted_intervals <- predict(model_cricoid,data.frame(x=age),interval='confidence', level=0.99)
measurements_predict = cbind(measurements, predicted_intervals)

p <- (ggplot(measurements_predict, aes(x=age.en.mois, y=AP.cricoïde)) + geom_point() + 
        geom_line(aes(y = fit), col='blue',size = 1) + geom_ribbon( aes(ymin = lwr, ymax = upr, colour = 'gray'), alpha = .15, linetype = 0)
      + theme_ipsum(grid=FALSE, axis=TRUE, axis_title_just='c') + 
        labs(x="Age (months)", y="Diameter (mm)", title="Anteroposterior Cricoid Diameters", caption=paste("AIC", AIC(model_cricoid))) +
        ylim(0, 25) + guides(colour=FALSE))
print(p)

ggsave('/Users/imag2/Desktop/Upper_Airways_Dimensions/results/cricoid.png', p)
