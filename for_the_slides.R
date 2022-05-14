






lm.model1 <- lm(covid.liguria, formula = terapia_intensiva ~ totale_positivi + colori)

lm.model2 <- lm(covid.liguria, formula = terapia_intensiva ~ totale_positivi + colori + variazione_totale_positivi)

lm.model3 <- lm(covid.liguria, formula = terapia_intensiva ~ totale_positivi + colori + variazione_totale_positivi + dimessi_guariti_giornaliero)

lm.model4 <- lm(covid.liguria, formula = terapia_intensiva ~ totale_positivi + colori + variazione_totale_positivi + deceduti_giornaliero)

lm.model5 <- lm(covid.liguria, formula = terapia_intensiva ~ totale_positivi + colori + variazione_totale_positivi + totale_vaccini)











