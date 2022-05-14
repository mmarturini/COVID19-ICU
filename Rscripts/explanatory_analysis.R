library(tidyverse)
library(PerformanceAnalytics)



# this link keeps aggiornarsi
covid.data <- read.csv("https://raw.githubusercontent.com/pcm-dpc/
COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv")

covid.liguria <- subset(covid.data, subset =  covid.data$denominazione_regione == "Liguria")

# #data from Sept 1 2020 to Feb 3 2021
# sept.covid.liguria <- covid.liguria[-c(1:190),]
# sept.covid.liguria <- sept.covid.liguria[-c(157:169),]
# 
# sept.covid.liguria <- sept.covid.liguria[-157,]
# 
# aux.liguria <- covid.liguria[-c(1:189),]
# aux.liguria <- aux.liguria[-c(158:169),]
# 
# aux.liguria <- aux.liguria[-159,]


#data from Sept1 to Feb 10 2021
sept.covid.liguria <- covid.liguria[-c(1:190),] # delete everything before september

sept.covid.liguria$data[163] # 10 Febbraio
Feb.covid.liguria <- sept.covid.liguria[-c(163:171),] #deleting from 10th Feb to today

aux.liguria <- covid.liguria[-c(1:189),]
aux.liguria <- aux.liguria[-c(164:171),]

unseen.data <- sept.covid.liguria[-c(1:163),]


# deleting non significant variables
braw.data <- subset(sept.covid.liguria, select = -c(denominazione_regione,lat,long,codice_regione,stato,codice_nuts_1,codice_nuts_2,note,note_test,note_casi))

# deleting not applicable
braw.data <- subset(raw.data, select = -c(ingressi_terapia_intensiva,casi_da_screening,casi_da_sospetto_diagnostico, tamponi_test_antigenico_rapido,tamponi_test_molecolare,totale_positivi_test_antigenico_rapido,totale_positivi_test_molecolare))


# considero numero giornaliero di dimessi_guariti e deceduti e non il totale cumulativo
#dimessi guariti

n <- 175

dimessi_guariti_giornaliero <- rep(0,n)
length(dimessi_guariti_giornaliero)

for (i in c(1:n)) {
  dimessi_guariti_giornaliero[i] = aux.liguria$dimessi_guariti[i+1]-aux.liguria$dimessi_guariti[i];
}

dimessi_guariti_giornaliero <- as.integer(dimessi_guariti_giornaliero)
length(dimessi_guariti_giornaliero)

#deceduti
deceduti_giornaliero <- rep(0,n)
length(deceduti_giornaliero)

for (i in c(1:n) ) {
  deceduti_giornaliero[i] = aux.liguria$deceduti[i+1]-aux.liguria$deceduti[i];
}

deceduti_giornaliero <- as.integer(deceduti_giornaliero)
length((deceduti_giornaliero))

braw.data <- cbind(raw.data, deceduti_giornaliero)
braw.data <- cbind(raw.data, dimessi_guariti_giornaliero)
braw.data <- subset(raw.data, select = -c(dimessi_guariti,deceduti))


#tolgo ulteriori correlate/non significative
# totale ospedalizzati perfettamente lineare con ricoverati con sintomi
raw.data <- subset(raw.data, select = -c(nuovi_positivi,isolamento_domiciliare, totale_casi,tamponi,casi_testati, totale_ospedalizzati))

View(raw.data)


# aggiungo i colori (per data fino a 10 Feb)

colori <- rep(1,66)
giallo1 <- rep(3,5)
arancio1 <- rep(3,18)
giallo2 <- rep(2,25)
rosso1 <- rep(4,4)
arancio2 <- rep(3,3)
rosso2 <- rep(4,4)
arancio3 <- 3
rossa3 <- rep(4,2)
giallo3 <- rep(2,10)
arancio4 <- rep(3,15)
#giallo4 <- rep(2,3) fino al 3 feb
giallo4 <- rep(2,9)
#arancio5 <- rep(3,4) fino al 13 feb
arancio5 <- rep(3,12) #fino a 21 Feb


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
#colori <- append(colori, giallo4)
colori <- append(colori,giallo4)
#colori <- append(colori, arancio5)
colori <- append(colori, arancio5)

length(colori)


colori <- as.factor(colori)
raw.data <- cbind(raw.data, colori)


raw.data$totale_positivi

#changing x axis, for the 10th Feb

indexes <- c(1:162)
indexes <- as.integer(indexes)
raw.data <- subset(raw.data, select = -data)
raw.data <- cbind(raw.data,indexes)

#in general
indexes <- c(1:n)
indexes <- as.integer(indexes)
raw.data <- subset(raw.data, select = -data)
raw.data <- cbind(raw.data,indexes)


#explanatory variables in funzione di  tempo

colors.pl <- ggplot(data = raw.data, mapping = aes(x=indexes,y=terapia_intensiva, color=colori)) + geom_point()
colors.pl + scale_color_manual(values=c("grey", "yellow", "orange", "red")) + 
  labs(title = "Terapia intensiva Liguria",x="Mesi",y="Terapia intensiva") +
  scale_x_continuous(breaks = seq(0,155,31), labels = c("Settembre","Ottobre","Novembre","Dicembre","Gennaio","Febbraio")) +
  geom_vline(xintercept = 55) +
  geom_vline(xintercept = 118) +
  geom_vline(xintercept = 139)



colors.pl <- ggplot(data = raw.data, mapping = aes(x=indexes,y=variazione_totale_positivi, color=colori)) + geom_point()
colors.pl + scale_color_manual(values=c("grey", "yellow", "orange", "red")) + 
  labs(title = "Totale positivi Liguria",x="Mesi") +
  scale_x_continuous(breaks = seq(0,155,31), labels = c("Settembre","Ottobre","Novembre","Dicembre","Gennaio","Febbraio"))


library(ggpubr)
# terapia intensiva in funzione di explanatory variables 

#intensiva vs ricoverati
ggplot(data = raw.data, mapping = aes(x=ricoverati_con_sintomi,y=terapia_intensiva, color=colori)) +
  geom_point() +
  scale_color_manual(values=c("grey", "yellow", "orange", "red"))


# vs totale_positivi
ggplot(data = raw.data, mapping = aes(x=totale_positivi,y=terapia_intensiva, color=colori)) +
  geom_point() +
  scale_color_manual(values=c("grey", "yellow", "orange", "red")) 

# vs var_tot_positivi
ggplot(data = raw.data, mapping = aes(x=variazione_totale_positivi,y=terapia_intensiva, color=colori)) +
  geom_point() +
  scale_color_manual(values=c("grey", "yellow", "orange", "red"))

# vs deceduti_giornaliero
ggplot(data = raw.data, mapping = aes(x=deceduti_giornaliero,y=terapia_intensiva, color=colori)) +
  geom_point() +
  scale_color_manual(values=c("grey", "yellow", "orange", "red"))

# vs dimessi_guariti
ggplot(data = raw.data, mapping = aes(x=dimessi_guariti_giornaliero,y=terapia_intensiva, color=colori)) +
  geom_point() +
  scale_color_manual(values=c("grey", "yellow", "orange", "red"))


?ggarrange


# vs indexes
ggplot(data = raw.data, mapping = aes(x=indexes,y=terapia_intensiva, color=colori)) +
  geom_point() +
  scale_color_manual(values=c("grey", "yellow", "orange", "red"))




#correlation chart
data <- subset(raw.data,select = -c(colori,indexes))
chart.Correlation(data)
# consider deleting one between ricoverati e totale_positivi?

chart.Correlation(sept.covid.liguria[, c("totale_ospedalizzati", "isolamento_domiciliare", "totale_positivi", "ricoverati_con_sintomi")])




ggplot(data,aes(x=ricoverati_con_sintomi,y=terapia_intensiva, color=colori)) +
  geom_point() +
  scale_color_manual(values=c("grey", "yellow", "orange", "red"))

chart.Correlation(data[,])







#actual data

indexes <- as.integer(c(1:173))
length(indexes)
sept.covid.liguria <- cbind(sept.covid.liguria, indexes)


colors.pl <- ggplot(data = sept.covid.liguria, mapping = aes(x=indexes,y=terapia_intensiva)) 
colors.pl + geom_point() +  
  geom_vline(xintercept =  162, linetype="dotted",color = "blue") +
  labs(title = "Terapia intensiva Liguria",x="Mesi",y="Terapia intensiva") +
  scale_x_continuous(breaks = seq(0,155,31), labels = c("Settembre","Ottobre","Novembre","Dicembre","Gennaio","Febbraio"))

