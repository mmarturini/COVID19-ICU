# let's try to consider a 7 days delay in order to take into account the lag
# of the effects of the decreti 
# the process to take the data and consider just some variables is the same,
# what changes is just the colors, that are shifted 7 days forward


colori <- rep(1,73)
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
giallo4 <- rep(2,2)

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
colori <- append(colori,giallo4)


length(colori)
colori <- as.factor(colori)


shift.data <- subset(raw.data,select = -colori)
shift.data <- cbind(shift.data, colori)


shift.model1 <- lm(shift.data, formula = terapia_intensiva ~ totale_positivi+variazione_totale_positivi+colori)
summary(shift.model1)
extractAIC(shift.model1)

par(mfrow=c(2,2))
plot(shift.model1)
vif(shift.model)

# with indexes
shift.model2 <- lm(shift.data, formula = terapia_intensiva ~ totale_positivi+variazione_totale_positivi+colori+indexes)
summary(shift.model2)
extractAIC(shift.model2)

par(mfrow=c(2,2))
plot(shift.model2)
vif(shift.model2)


shift.predictions <- predict(shift.model2, newdata = unseen.data)
shift.predictions <- as.integer(round(shift.predictions))

shift.newdata = unseen.data
shift.newdata <- subset(shift.newdata, select = -terapia_intensiva)
shift.newdata <- cbind(shift.newdata, shift.predictions)


# predictions as if liguria is yellow
colors.pl <- ggplot(data = raw.data, mapping = aes(x=indexes,y=terapia_intensiva, color=colori)) 
colors.pl + geom_point() +  
  geom_vline(xintercept =  162, linetype="dotted",color = "blue") +
  geom_point(data=shift.newdata, aes(x=indexes,y=shift.predictions)) +
  geom_point(data = unseen.data, aes(x=indexes, y=terapia_intensiva), color = "blue") +
  scale_color_manual(values=c("grey", "yellow", "orange", "red")) + 
  labs(title = "Terapia intensiva Liguria",x="Mesi",y="Terapia intensiva") +
  scale_x_continuous(breaks = seq(0,155,31), labels = c("Settembre","Ottobre","Novembre","Dicembre","Gennaio","Febbraio"))



#predictions as if liguria were
levels(shift.newdata$colori) <- 3
colors.pl <- ggplot(data = raw.data, mapping = aes(x=indexes,y=terapia_intensiva, color=colori)) 
colors.pl + geom_point() +  
  geom_vline(xintercept =  162, linetype="dotted",color = "blue") +
  geom_point(data=shift.newdata, aes(x=indexes,y=shift.predictions)) +
  geom_point(data = unseen.data, aes(x=indexes, y=terapia_intensiva), color = "blue") +
  scale_color_manual(values=c("grey", "yellow", "orange", "red")) + 
  labs(title = "Terapia intensiva Liguria",x="Mesi",y="Terapia intensiva") +
  scale_x_continuous(breaks = seq(0,155,31), labels = c("Settembre","Ottobre","Novembre","Dicembre","Gennaio","Febbraio"))







