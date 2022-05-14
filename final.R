library(tidyverse)
library(PerformanceAnalytics)


covid.data <- read.csv("https://raw.githubusercontent.com/pcm-dpc/
COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv")

covid.liguria <- subset(covid.data, subset =  covid.data$denominazione_regione == "Liguria")


sept.covid.liguria <- covid.liguria[-c(1:190),]
sept.covid.liguria <- sept.covid.liguria[-c(157:169),]
#from Jan 1st to Feb 3rd

sept.covid.liguria <- sept.covid.liguria[-157,]

aux.liguria <- covid.liguria[-c(1:189),]
aux.liguria <- aux.liguria[-c(158:169),]

aux.liguria <- aux.liguria[-159,]

ggplot(data = sept.covid.liguria, mapping = aes(x=data,y = terapia_intensiva)) + geom_point()
ggplot(data = sept.covid.liguria, mapping = aes(x=data,y = totale_ospedalizzati)) + geom_point()




#not significant
raw.data <- subset(sept.covid.liguria, select = -c(denominazione_regione,lat,long,codice_regione,stato,codice_nuts_1,codice_nuts_2,note,note_test,note_casi))
summary(raw.data)

#not applicable
raw.data <- subset(raw.data, select = -c(ingressi_terapia_intensiva,casi_da_screening,casi_da_sospetto_diagnostico, tamponi_test_antigenico_rapido,tamponi_test_molecolare,totale_positivi_test_antigenico_rapido,totale_positivi_test_molecolare))
summary(raw.data)

ggplot(data = raw.data, mapping = aes(x=data,y = terapia_intensiva)) + geom_point()
ggplot(data = raw.data, mapping = aes(x=data,y = totale_ospedalizzati)) + geom_point()

ggplot(raw.data, aes(x=data, y=deceduti)) + geom_point()
ggplot(raw.data, aes(x=data, y=dimessi_guariti)) + geom_point()
ggplot(raw.data, aes(x=data, y=totale_casi)) + geom_point()
ggplot(raw.data, aes(x=data, y=tamponi)) + geom_point()
ggplot(raw.data, aes(x=data, y=dimessi_guariti)) + geom_point()




#Correlation
#raw.data <- subset(raw.data, select = -c(data, totale_ospedalizzati, ))

#elimino totale ospedalizzati perche perfettamente lineare con ricoverati con sintomi


# considere numero giornaliero non il totale
class(raw.data$dimessi_guariti) #vector, partono da 1 non da zero
length(raw.data$dimessi_guariti)

#dimessi guariti
dimessi_guariti_giornaliero <- rep(0,156)
length(dimessi_guariti_giornaliero)
dimessi_guariti_giornaliero[156]

for (i in c(1:length(raw.data$dimessi_guariti)) ) {
  dimessi_guariti_giornaliero[i] = aux.liguria$dimessi_guariti[i+1]-aux.liguria$dimessi_guariti[i];
}

plot(dimessi_guariti_giornaliero)
dimessi_guariti_giornaliero <- as.integer(dimessi_guariti_giornaliero)
length(dimessi_guariti_giornaliero)

#deceduti
deceduti_giornaliero <- rep(0,length(raw.data$deceduti))
length(deceduti_giornaliero)

for (i in c(1:length(raw.data$deceduti)) ) {
  deceduti_giornaliero[i] = aux.liguria$deceduti[i+1]-aux.liguria$deceduti[i];
}

plot(deceduti_giornaliero)
deceduti_giornaliero <- as.integer(deceduti_giornaliero)
length((deceduti_giornaliero))

# #tamponi
# tamponi_giornaliero <- rep(0,length(raw.data$tamponi))
# length(tamponi_giornaliero)
# 
# for (i in c(1:length(raw.data$tamponi)) ) {
#   tamponi_giornaliero[i] = aux.liguria$tamponi[i+1]-aux.liguria$tamponi[i];
# }
# 
# plot(tamponi_giornaliero)
# tamponi_giornaliero <- as.integer(tamponi_giornaliero)




raw.data <- cbind(raw.data, deceduti_giornaliero)
raw.data <- cbind(raw.data, dimessi_guariti_giornaliero)
raw.data <- subset(raw.data, select = -c(dimessi_guariti,deceduti))


#tolgo le cumulative
raw.data <- subset(raw.data, select = -c(totale_casi,tamponi,casi_testati))
chart.Correlation(raw.data)

#tolgo le correlate
raw.data <- subset(raw.data, select = -c(nuovi_positivi,isolamento_domiciliare))
chart.Correlation((raw.data))



#adding colors
colori <- rep(1,66)
colori


giallo1 <- rep(2,5)
arancio1 <- rep(3,18)
giallo2 <- rep(2,25)
rosso1 <- rep(4,4)

arancio2 <- rep(3,3)
rosso2 <- rep(4,4)
arancio3 <- 3
rossa3 <- rep(4,2)
giallo3 <- rep(2,10)
arancio4 <- rep(3,15)
giallo4 <- rep(2,3)


colori <- append(colori, giallo1)
colori <- append(colori, arancio1)
colori <- append(colori, giallo2)
colori <- append(colori, rosso1)

colori <- append(colori, arancio2)
colori <- append(colori, rosso2)
colori <- append(colori, arancio3)
colori <- append(colori, rossa3)
colori <- append(colori, giallo3)
colori <- append(colori, arancio4)
colori <- append(colori, giallo4)



length(colori)


colori[72:89]
colori[90:114]
colori[115:118]


colori <- as.factor(colori)
str(colori)

raw.data <- cbind(raw.data, colori)


#senza la data
View(raw.data)
str(raw.data)
chart.Correlation(raw.data)
