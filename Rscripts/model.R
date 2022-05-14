#Bougammoura, Zasa, Bertone, Irto --> Puglia, Intensive Cares Units.
#GROUP C: Orzan, Blasone, Crognaletti, Marchiori Pietrosanti --> Lombardia, Intensive Cares Units.


data <- subset(raw.data,select = -c(colori))
chart.Correlation(data)


?glm

glm.model <- glm(data = raw.data, formula = terapia_intensiva~totale_positivi+colori+deceduti_giornaliero+ricoverati_con_sintomi, family = gaussian)
summary(model)

par(mfrow=c(2,2))
plot(model)

lm.model <- lm(data = raw.data, formula = terapia_intensiva~ ricoverati_con_sintomi+colori+totale_positivi)
summary(lm.model)
extractAIC(lm.model)

par(mfrow=c(2,2))
plot(lm.model)
extractAIC(model)

?extractAIC

library(mgcv)
model2 <- gam(formula = terapia_intensiva ~ totale_positivi+colori, data = raw.data)
summary(model2)
par(mfrow=c(2,2))
plot(model2)
extractAIC(model2)
