library(ggplot2)
library(hrbrthemes)
library(factoextra)
library("RColorBrewer")

rm(list = ls(all.names = TRUE)) 
gc()

# Dataset ----

ds <- na.omit(read.csv('measurements.csv', sep=';', dec=","))
measurements = ds[,4:length(ds)]

smp_size <- floor(0.8 * nrow(measurements))
set.seed(42)

train_ind <- sample(seq_len(nrow(measurements)), size = smp_size)
train <- measurements[train_ind, ]
test <- measurements[-train_ind, ]

age_train = train[,1]
age_test = test[,1]

# Glottic Model ----

ap_glottic_diameter_train = train[,9]
ap_glottic_diameter_test = test[,9]

age <- age_train
model_glottic <- lm(ap_glottic_diameter_train ~ poly(age,3))

age <- age_test
predicted_intervals <- predict(model_glottic,data.frame(x=age),interval='confidence', level=0.99)
measurements_predict = cbind(test, predicted_intervals)

d = ap_glottic_diameter_test-predicted_intervals[,1]
mse_glottic = mean((d)^2)
mae_glottic = mean(abs(d))
rmse_glottic = sqrt(mse_glottic)

p <- (ggplot(measurements_predict, aes(x=age.en.mois, y=antéro.post.CV)) + geom_point() + 
        geom_line(aes(y = fit), col='blue',size = 1) + geom_ribbon( aes(ymin = lwr, ymax = upr, colour = 'gray'), alpha = .15, linetype = 0)
      + labs(x="Age (months)", y="Diameter (mm)", title="Anteroposterior Glottic Diameters", caption=paste("AIC", AIC(model_glottic))) +
        ylim(0, 25) + guides(colour=FALSE))
print(p)

ggsave('/Users/imag2/Desktop/Upper_Airways_Dimensions/results/glottic.png', p)

# Cricoid Model ----

ap_cricoid_diameter_train = train[,10]
ap_cricoid_diameter_test = test[,10]

age <- age_train
model_cricoid <- lm(ap_cricoid_diameter_train ~ poly(age,3))
#summary(model_cricoid)
#confint(model_cricoid, level=0.95)

age <- age_test
predicted_intervals <- predict(model_cricoid,data.frame(x=age),interval='confidence', level=0.99)
measurements_predict = cbind(test, predicted_intervals)

d = ap_cricoid_diameter_test-predicted_intervals[,1]
mse_cricoid = mean((d)^2)
mae_cricoid = mean(abs(d))
rmse_cricoid = sqrt(mse_glottic)

p <- (ggplot(measurements_predict, aes(x=age.en.mois, y=AP.cricoïde)) + geom_point() + 
        geom_line(aes(y = fit), col='blue',size = 1) + geom_ribbon( aes(ymin = lwr, ymax = upr, colour = 'gray'), alpha = .15, linetype = 0)
<<<<<<< HEAD
      + theme_ipsum(grid=FALSE, axis=TRUE, axis_title_just='c') + 
        labs(x="Age (months)", y="Diameter (mm)", title="Anteroposterior Cricoid Diameters", caption=paste("AIC", AIC(model_cricoid))) +
=======
      + labs(x="Age (months)", y="Diameter (mm)", title="Anteroposterior Glottic Diameters", caption=paste("AIC", AIC(model_cricoid))) +
>>>>>>> cfa63f3befcabd5d8e2bccbce596e8768f45cd9d
        ylim(0, 25) + guides(colour=FALSE))
print(p)

ggsave('/Users/imag2/Desktop/Upper_Airways_Dimensions/results/cricoid.png', p)
