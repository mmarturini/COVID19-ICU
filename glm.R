?glm

#totale positivi and colors
glm.model1 <- glm(data = raw.data, formula = terapia_intensiva ~ totale_positivi, family = poisson)
summary(glm.model1)
extractAIC(glm.model1)

par(mfrow=c(2,2))
plot(glm.model1)


#interaction term
glm.model2 <- glm(raw.data, formula = terapia_intensiva ~ totale_positivi+I(totale_positivi^2)+colori, family = poisson)
summary(glm.model2)
extractAIC(glm.model2)

par(mfrow=c(2,2))
plot(glm.model2)


# variazione totali positivi
glm.model3 <- glm(raw.data, formula = terapia_intensiva ~ totale_positivi+colori+variazione_totale_positivi, family = poisson)
summary(glm.model3)
extractAIC(glm.model3)

par(mfrow=c(2,2))
plot(glm.model3)

library(DAAG)
vif(glm.model3)

# + ricoverati con sintomi --> multicollinearità
glm.model4 <- glm(raw.data, formula = terapia_intensiva ~ totale_positivi+colori+variazione_totale_positivi+ricoverati_con_sintomi, family = poisson)
summary(glm.model4)
extractAIC(glm.model4)

par(mfrow=c(2,2))
plot(glm.model4)

vif(glm.model4) #anche in questo caso ocnfermata multicollinearità con ricoverati con sintomi


# - ric von sintomi + dimessi_guariti
glm.model5 <- glm(raw.data, formula = terapia_intensiva ~ totale_positivi+colori+variazione_totale_positivi+dimessi_guariti_giornaliero, family = poisson)
summary(glm.model5)
extractAIC(glm.model5)

par(mfrow=c(2,2))
plot(glm.model5)
vif(glm.model5)

# con glm poisson sembra che dimessi guariti abbiano un'influenza nel predire le terapie intensive



# - dimessi + morti
glm.model6 <- glm(raw.data, formula = terapia_intensiva ~ totale_positivi+colori+variazione_totale_positivi+deceduti_giornaliero, family = poisson)
summary(glm.model6)
extractAIC(lm.model6)

par(mfrow=c(2,2))
plot(glm.model6)
vif(glm.model6)


#both dimessi_guariti e deceduti
glm.model7 <- glm(raw.data, formula = terapia_intensiva ~ totale_positivi+colori+variazione_totale_positivi+deceduti_giornaliero+dimessi_guariti_giornaliero, family = poisson)
summary(glm.model7)

par(mfrow=c(2,2))
plot(glm.model7)
vif(glm.model7)


# indexes (time as days passed)
glm.model8 <- glm(raw.data, formula = terapia_intensiva ~ totale_positivi+colori+variazione_totale_positivi+indexes, family = poisson)
summary(glm.model8)

par(mfrow=c(2,2))
plot(glm.model8)
vif(glm.model8)

#without var tot positivi
glm.model9 <- glm(raw.data, formula = terapia_intensiva ~ totale_positivi+colori+indexes, family = poisson)
summary(glm.model9)

par(mfrow=c(2,2))
plot(glm.model9)


# con variazione pos e tot pos al quadrato
glm.model10 <- glm(raw.data, formula = terapia_intensiva ~ totale_positivi+I(totale_positivi^2)+colori+variazione_totale_positivi+indexes, family = poisson(link = "identity"))
summary(glm.model10)

par(mfrow=c(2,2))
plot(glm.model10)
vif(glm.model10)


glm10.predictions <- predict.glm(object = glm.model10, newdata = unseen.data)
glm10.predictions


# quasi???
glm.model11 <- glm(raw.data, formula = terapia_intensiva ~ totale_positivi+colori+variazione_totale_positivi+indexes, family = quasi)
summary(glm.model11)

par(mfrow=c(2,2))
plot(glm.model10)
vif(glm.model10)


glm10.predictions <- predict.glm(object = glm.model10, newdata = unseen.data)
glm10.predictions

