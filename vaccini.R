library(tidyverse)


vaccini <- read.csv("https://raw.githubusercontent.com/italia/covid19-opendata-vaccini/master/dati/somministrazioni-vaccini-summary-latest.csv")
vaccini.liguria <- subset(vaccini, subset =  vaccini$area == "LIG")
vaccini.liguria <- vaccini.liguria[order(vaccini.liguria$data_somministrazione),] # ordering by data_somministrazione

#deleting non important covariates
vaccini.liguria <- subset(vaccini.liguria, select = -c(area,nome_area,prima_dose,seconda_dose,sesso_maschile,sesso_femminile,codice_NUTS1,codice_NUTS2))


# considering the totale cumulativo of vaccini
n <- 58

totale_cumulativo <- rep(0,n)
totale_cumulativo[1] = vaccini.liguria$totale[1]

for (i in c(2:n)) {
  totale_cumulativo[i] = totale_cumulativo[i-1]+vaccini.liguria$totale[i];
}


#personale sanitario totale cumulativo
personale_sanitario_totale <- rep(0,n)
personale_sanitario_totale[1] = vaccini.liguria$categoria_operatori_sanitari_sociosanitari[1]

for (i in c(2:n)) {
  personale_sanitario_totale[i] = personale_sanitario_totale[i-1]+ vaccini.liguria$categoria_operatori_sanitari_sociosanitari[i];
}


#personale non sanitario totale cumulativo
personale_non_sanitario_totale <- rep(0,n)
personale_non_sanitario_totale[1] = vaccini.liguria$categoria_personale_non_sanitario[1]

for (i in c(2:n)) {
  personale_non_sanitario_totale[i] = personale_non_sanitario_totale[i-1]+ vaccini.liguria$categoria_personale_non_sanitario[i];
}


# ospiti RSA totale cumulativo
ospiti_RSA <- rep(0,n)
ospiti_RSA[1] = vaccini.liguria$categoria_ospiti_rsa[1]

for (i in c(2:n)) {
  ospiti_RSA[i] = ospiti_RSA[i-1]+ vaccini.liguria$categoria_ospiti_rsa[i];
}


# over 80 totale cumulativo
over_80 <- rep(0,n)
over_80[1] = vaccini.liguria$categoria_over80[1]

for (i in c(2:n)) {
  over_80[i] = over_80[i-1]+ vaccini.liguria$categoria_over80[i];
}



# adding totali cumulativi
vaccini.liguria <- cbind(vaccini.liguria, totale_cumulativo, personale_sanitario_totale, personale_non_sanitario_totale, ospiti_RSA, over_80)

indexes <- c(1:56)
vaccini.liguria <- cbind(vaccini.liguria, indexes)


vaccini.liguria$data_somministrazione[47] # 11 Febbraio 

my.vax <- vaccini.liguria$totale_cumulativo[47:58]
length(my.vax)
my.vax <- as.integer(my.vax)


plot.vacci <- subset(vaccini.liguria, select = -c(data_somministrazione,totale,categoria_operatori_sanitari_sociosanitari, categoria_personale_non_sanitario, categoria_ospiti_rsa, categoria_over80, codice_regione_ISTAT ))
library(reshape2)
dd = melt(plot.vacci, id=c("indexes"))

ggplot(dd) + geom_line(aes(x=indexes, y=value, colour=variable), size = 1) +
  scale_colour_manual(values=c("red","blue","deepskyblue","orange","forestgreen")) +
  scale_x_continuous(breaks = seq(6,56,31), labels = c("Gennaio","Febbraio")) +
  labs(title = "Vaccinazioni Liguria",
       x="Mesi", 
       y="Totali cumulativi")



#let's fit a model only from january onward

sept.covid.liguria$data[118] # 27 Dicembre. primo giorno vaccinazioni
vaccini.data <- sept.covid.liguria[-c(1:117),]
vaccini.data$data[51]  # 15 Febbraio
vaccini.data <- vaccini.data[-c(52:56),]


# deleting non significant variables
vaccini.data <- subset(vaccini.data, select = -c(denominazione_regione,lat,long,codice_regione,stato,codice_nuts_1,codice_nuts_2,note,note_test,note_casi))

# deleting not applicable
vaccini.data <- subset(vaccini.data, select = -c(ingressi_terapia_intensiva,casi_da_screening,casi_da_sospetto_diagnostico, tamponi_test_antigenico_rapido,tamponi_test_molecolare,totale_positivi_test_antigenico_rapido,totale_positivi_test_molecolare))

vaccini.data <- subset(vaccini.data, select = -c(nuovi_positivi,isolamento_domiciliare, totale_casi,tamponi,casi_testati, totale_ospedalizzati))
vaccini.data <- subset(vaccini.data, select = -c(dimessi_guariti,deceduti))


colori <- 4      #27 Dicembre
arancio2 <- rep(3,3)
rosso2 <- rep(4,4)
arancio3 <- 3
rossa3 <- rep(4,2)
giallo3 <- rep(2,10)
arancio4 <- rep(3,15)
giallo4 <- rep(2,13)  #fino a 9 Febbraio
arancio5 <- rep(3,2)

colori <- append(colori, arancio2)
colori <- append(colori, rosso2)
colori <- append(colori, arancio3)
colori <- append(colori, rossa3)
colori <- append(colori, giallo3)
colori <- append(colori, arancio4)
colori <- append(colori, giallo4)
colori <- append(colori, arancio5)

length(colori)
colori <- as.factor(colori)


indexes <- as.integer(c(1:51))
length(indexes)


vaccini.data <- cbind(vaccini.data, colori, indexes)



totale_vaccini <- vaccini.liguria$totale_cumulativo 
totale_vaccini <- totale_vaccini[-c(52:56)]
length(totale_vaccini)

vaccini.data <- cbind(vaccini.data, totale_vaccini)


#plotting
vax.plot <- ggplot(data = vaccini.data, mapping = aes(x=indexes,y=terapia_intensiva, color=colori)) + geom_point()
vax.plot + scale_color_manual(values=c("yellow", "orange", "red")) + 
  labs(title = "Terapia intensiva Liguria",x="Mesi",y="Terapia intensiva") +
  scale_x_continuous(breaks = seq(6,56,31), labels = c("Gennaio","Febbraio"))


ggplot(data = vaccini.data, mapping = aes(x=totale_positivi,y=terapia_intensiva, color=colori)) +
  geom_point() +
  scale_color_manual(values=c("yellow", "orange", "red"))

ggplot(data = vaccini.data, mapping = aes(x=totale_vaccini,y=terapia_intensiva, color=colori)) +
  geom_point() +
  scale_color_manual(values=c("yellow", "orange", "red"))



#model
vaccini.lm <- lm(data = vaccini.data, formula = terapia_intensiva ~ totale_positivi+colori+totale_vaccini)
summary(vaccini.lm)
extractAIC(vaccini.lm)

par(mfrow=c(2,2))
plot(vaccini.lm)

chart.Correlation(vaccini.data)
str(vaccini.data)


#no colors
vaccini.lm2 <- lm(data = vaccini.data, formula = terapia_intensiva ~ totale_positivi+totale_vaccini+ricoverati_con_sintomi)
summary(vaccini.lm2)
extractAIC(vaccini.lm2)

par(mfrow=c(2,2))
plot(vaccini.lm2)



