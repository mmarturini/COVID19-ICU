#linear models

#totale positivi and colors
lm.model1 <- lm(raw.data, formula = terapia_intensiva ~ totale_positivi+colori)
summary(lm.model1)
extractAIC(lm.model1)

par(mfrow=c(2,2))
plot(lm.model1)


#interaction term
lm.model2 <- lm(raw.data, formula = terapia_intensiva ~ totale_positivi*colori)
summary(lm.model2)
extractAIC(lm.model2)

par(mfrow=c(2,2))
plot(lm.model2)


# variazione totali positivi
lm.model3 <- lm(raw.data, formula = terapia_intensiva ~ totale_positivi+colori+variazione_totale_positivi)
summary(lm.model3)
extractAIC(lm.model3)

par(mfrow=c(2,2))
plot(lm.model3)

library(DAAG)
library(kableExtra)
vif(lm.model3)

# + ricoverati con sintomi --> multicollinearità
lm.model4 <- lm(raw.data, formula = terapia_intensiva ~ totale_positivi+colori+variazione_totale_positivi+ricoverati_con_sintomi)
summary(lm.model4)
extractAIC(lm.model4)

par(mfrow=c(2,2))
plot(lm.model4)

vif(lm.model4)


# - ric von sintomi + dimessi_guariti
lm.model5 <- lm(raw.data, formula = terapia_intensiva ~ totale_positivi+colori+variazione_totale_positivi+dimessi_guariti_giornaliero)
summary(lm.model5)
extractAIC(lm.model5)

par(mfrow=c(2,2))
plot(lm.model5)
vif(lm.model5)

# - dimessi + morti
lm.model6 <- lm(raw.data, formula = terapia_intensiva ~ totale_positivi+colori+variazione_totale_positivi+deceduti_giornaliero)
summary(lm.model6)
extractAIC(lm.model6)

par(mfrow=c(2,2))
plot(lm.model6)
vif(lm.model6)


# indexes (time as days passed)
lm.model7 <- lm(raw.data, formula = terapia_intensiva ~ totale_positivi+colori+variazione_totale_positivi+indexes)
summary(lm.model7)
extractAIC(lm.model7)

par(mfrow=c(2,2))
plot(lm.model7)
vif(lm.model7)

m7.predictions <- predict.lm(lm.model7,unseen.data)



#ANOVA
anova(lm.model1,lm.model3, lm.model7)



#lm con vaccini
totale_vaccini <- rep(0,106)
totale_vaccini <- append(totale_vaccini, as.integer(vaccini.liguria$totale_cumulativo))

length(totale_vaccini)

raw.data <- cbind(raw.data, totale_vaccini)

lm.vaccini <- lm(data = raw.data, formula = terapia_intensiva ~ totale_positivi+colori+variazione_totale_positivi+totale_vaccini)
summary(lm.vaccini)
extractAIC(lm.vaccini)

par(mfrow=c(2,2))
plot(lm.vaccini)


# Anova
anova(lm.model1,lm.model3,lm.vaccini)
anova(lm.model3, lm.vaccini)

# MODEL FIT
fit <- predict(lm.vaccini, raw.data)
fit

raw.data <- cbind(raw.data,fit)

colors.pl <- ggplot(data = raw.data, mapping = aes(x=indexes,y=terapia_intensiva, color=colori)) + geom_point()
colors.pl + scale_color_manual(values=c("grey", "yellow", "orange", "red")) + 
  labs(title = "Terapia intensiva Liguria",x="Mesi",y="Terapia intensiva") +
  scale_x_continuous(breaks = seq(0,155,31), labels = c("Settembre","Ottobre","Novembre","Dicembre","Gennaio","Febbraio")) +
  geom_line(mapping = aes(y=fit), color = "red", size=1)




# UNSEEN DATA
unseen.data <- subset(unseen.data, select = -c(denominazione_regione,lat,long,codice_regione,stato,codice_nuts_1,codice_nuts_2,note,note_test,note_casi))
unseen.data <- subset(unseen.data, select = -c(ingressi_terapia_intensiva,casi_da_screening,casi_da_sospetto_diagnostico, tamponi_test_antigenico_rapido,tamponi_test_molecolare,totale_positivi_test_antigenico_rapido,totale_positivi_test_molecolare))
unseen.data <- subset(unseen.data, select = -c(nuovi_positivi,totale_ospedalizzati,totale_casi,tamponi,casi_testati,data,isolamento_domiciliare))

n <- 12

dimessi_guariti_giornaliero <- rep(0,n)
length(dimessi_guariti_giornaliero)

for (i in c(1:n) ) {
  dimessi_guariti_giornaliero[i] = aux.liguria$dimessi_guariti[i+1]-aux.liguria$dimessi_guariti[i];
}

dimessi_guariti_giornaliero <- as.integer(dimessi_guariti_giornaliero)
length(dimessi_guariti_giornaliero)

#deceduti
deceduti_giornaliero <- rep(0,n)
length(deceduti_giornaliero)

for (i in c(1:n)) {
  deceduti_giornaliero[i] = aux.liguria$deceduti[i+1]-aux.liguria$deceduti[i];
}

deceduti_giornaliero <- as.integer(deceduti_giornaliero)
length((deceduti_giornaliero))

unseen.data <- cbind(unseen.data, deceduti_giornaliero)
unseen.data <- cbind(unseen.data, dimessi_guariti_giornaliero)
unseen.data <- subset(unseen.data, select = -c(dimessi_guariti,deceduti))


giallo <- rep(2,4)
arancio <- rep(3,8)

colori <- append(giallo,arancio)
length(colori)

colori <- as.factor(colori) #let's suppose Liguria remains gialla
length(colori)
indexes <- as.integer(c(163:174))
length(indexes)

unseen.data <- cbind(unseen.data,colori)
unseen.data <- cbind(unseen.data,indexes)
str(unseen.data)

totale_vaccini <- my.vax
unseen.data <- cbind(unseen.data, totale_vaccini)

raw.data <- subset(raw.data, select = -fit)


# NOW predict
vax.predictions <- predict.lm(lm.vaccini,unseen.data)
vax.predictions

unseen.data <- cbind(unseen.data, vax.predictions)

colors.pl <- ggplot(data = raw.data, mapping = aes(x=indexes,y=terapia_intensiva, color=colori)) 
colors.pl + geom_point() +
  geom_point(data=unseen.data, aes(y=vax.predictions),color="blue") +
  scale_color_manual(values=c("grey", "yellow", "orange", "red")) + 
  labs(title = "Terapia intensiva Liguria",x="Mesi",y="Terapia intensiva") +
  scale_x_continuous(breaks = seq(0,155,31), labels = c("Settembre","Ottobre","Novembre","Dicembre","Gennaio","Febbraio")) +
  geom_vline(xintercept = 163,  linetype="dashed") +
  geom_point(data=bingo,aes(x=x,y=y), color="orange")


bingo <- data.frame(x=163:174,y=c(56,62,61,62,59,61,60,59,58,55,57,56))


# alternative, consider 3 diversi regimi
sept.covid.liguria$data[77]
fasi <- rep(0,77)

sept.covid.liguria$data[107]
rallentamento <- rep(1,30)

stabilità <- rep(2,55)

fasi <- append(fasi,rallentamento)
fasi <- append(fasi,stabilità)

length(fasi)

fasi <- as.factor(fasi)
levels(fasi) <- c("Inizio_ondata","Rallentamento","Stabilità")
levels(fasi)

new.data = raw.data
new.data <- cbind(new.data,fasi)

ggplot(data = new.data, mapping = aes(x=totale_positivi,y=terapia_intensiva, color=fasi)) +
  geom_point() 

ggplot(data = new.data, mapping = aes(x=variazione_totale_positivi,y=terapia_intensiva, color=fasi)) +
  geom_point() 

colors.pl <- ggplot(data = raw.data, mapping = aes(x=indexes,y=terapia_intensiva, color=fasi)) + geom_point()
colors.pl + labs(title = "Terapia intensiva Liguria",x="Mesi",y="Terapia intensiva") +
  scale_x_continuous(breaks = seq(0,155,31), labels = c("Settembre","Ottobre","Novembre","Dicembre","Gennaio","Febbraio"))


# totale positivi e fasi
lm.model8 <- lm(new.data, formula = terapia_intensiva ~ totale_positivi+fasi)
summary(lm.model8)
extractAIC(lm.model)

par(mfrow=c(2,2))
plot(lm.model8)
vif(lm.model8)


#totale positivi, fasi e var positivi
lm.model8 <- lm(new.data, formula = terapia_intensiva ~ totale_positivi+fasi+variazione_totale_positivi+indexes)
summary(lm.model8)
extractAIC(lm.model)

par(mfrow=c(2,2))
plot(lm.model8)
vif(lm.model8)


#indexes and indexes 
new.data <- subset(new.data, select = -c(ricoverati_con_sintomi, deceduti_giornaliero, dimessi_guariti_giornaliero,colori))
lm.model9 <- lm(new.data, formula = terapia_intensiva ~ .)
summary(lm.model9)
extractAIC(lm.model9)

par(mfrow=c(2,2))
plot(lm.model9)
vif(lm.model8)


























