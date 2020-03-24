#En attendant de corriger avec les années et les mois
dmy<-dmy(data$Période)
table(year(dmy))
table(month(dmy))
table(quarter(dmy))
# Make the years, months and quarters be factors
data$year <- year(dmy)
data$month <- month(dmy)
data$quarter <- quarter(dmy)
# Remove the two unnecessary columns
data <- data[, !(names(data) %in% c("Période", "trimestre"))]



#Ajout de variables repertoriant les numéros absolus des mois
data$month_abs_num <- ifelse(data$year == 2003, data$month, data$month + 12)
# Add a column with quarter's absolute number
data$quarter_abs_num <- ifelse(data$year == 2003, data$quarter, data$quarter + 4)




summary(data$Temps.moyen.de.conversation.sur.appels.sortants)
str(data)
as.numeric(data$temps.total.conversation.sur.appels.sortants)


#Les variables nombre sur le téléphone sont laissées de côté de sorte à endiguer le problème
#des individus à temps plein/ à temps partiel.En effet, on supposera que les individus à temps
#partiel s'ils avaient été dans une autre situation auraient eu les mêmes stats que les autres

#De ce fait, au niveau des variables de téléphonie, les variables retenues sont:
#ratio_appels_entrants, ratio_appels_sortants, ratio_wrap-up, les temps moy. de conversation(in/out)
#(éventuellement un taux de prise d'appel)
summary(data$Nombre.de.semaines)
data$Nombre.de.semaines<-as_factor(data$Nombre.de.semaines)
summary(data$Nombre.de.semaines)

#La plupart des individus de l'étude ont été évalués sur des mois de 4 semaines tandis que
#d'autres étaient sur des mois de 5 semaines
str(data)

#Analyses sur le nombre d'appels sortants, prise d'appels par heure, durée appels sortants/ entrants,
#temps total de conversation, temps total de retrait


#Analyse sur le nombre d'appels sortants
d1<-boxplot(data$nb.appels.sortants,main="Boxplot sur le nombre d'appels sortants effectués
            par les agents", ylab="Nombre d'appels effectués")
outlierd1<-boxplot.stats(data$nb.appels.sortants)$out
outlierd1

outlierd1.index<-which(data$nb.appels.sortants %in% c(outlierd1))
outlierd1.index
#Analyse sur le nombre d'appels pris par heure de disponibilité
d2<-boxplot(data$nombre.d.appels.pris.par.heure.de.disponibilité,main="Boxplot sur le nombre
            d'appels pris par heure de disponibilité des agents", ylab="Nombre d'appels pris par
            heure de disponibilité")

outlierd2<-boxplot.stats(data$nombre.d.appels.pris.par.heure.de.disponibilité)$out
outlierd2

outlierd2.index<-which(data$nombre.d.appels.pris.par.heure.de.disponibilité %in% c(outlierd2))
outlierd2.index
summary(data)

#Ajout de détection d'outliers sur le temps total de conversation
d3<-boxplot(data$temps.total.de.conversation,main="Boxplot sur le temps total de conversation")
outlierd3<-boxplot.stats(data$temps.total.de.conversation)$out
outlierd3.index<-which(data$temps.total.de.conversation %in% c(outlierd3))          

#Détection sur la durée des appels par jour
d4<-boxplot(data$durée.appels.entrants.jour, main="Boxplot sur la durée d'appels entrants
            par jour")#RAS
d5<-boxplot(data$durée.appels.sortants.jour, main="Boxplot sur la durée d'appels
            sortants par jour")



##########

outlier.d5<-boxplot.stats(data$durée.appels.sortants.jour)$out
outlierd5.index<-which(data$durée.appels.sortants.jour %in% c(outlier.d5))
outlierd5.index

#Détection sur le temps total de retrait
d6<-boxplot(data$temps.total.de.retrait, main="Temps total de retrait")
outlierd6<-boxplot.stats(data$temps.total.de.retrait)$out
outlierd6
outlierd6.index<-which(data$temps.total.de.retrait %in% c(outlierd6))
outlierd6.index

#Détection sur le temps total de disponibilité
d7<-boxplot(data$temps.total.de.disponibilité, main="Boxplot sur le temps total
            de disponibilité des agents")
outlierd7<-boxplot.stats(data$temps.total.de.disponibilité)$out
outlierd7.index<-which(data$temps.total.de.disponibilité %in% c(outlierd7))
outlierd7
outlierd7.index

#Détection sur le temps moyen de conversation sur appels sortants
d8<-boxplot(data$temps.moyen.de.conversation.sur.appels.sortants,
            col = "blue", main="Boxplot sur le temps moyen
            de conversation sur appels sortants",ylab="Temps moyen de conversation sur apppels
            sortants")
outlierd8<-boxplot.stats(data$temps.moyen.de.conversation.sur.appels.sortants)$out
outlierd8
outlierd8.index<-which(data$temps.moyen.de.conversation.sur.appels.sortants %in% c(outlierd8))
outlierd8.index

summary(data$temps.moyen.de.conversation.sur.appels.sortants)
summary(data$temps.moyen.de.conversation.sur.appels.entrants)

#Création de data2 en supprimant les outliers au niveau des appels sortants et du ratio appels pris
#heure de disponibilité
##############################
#############################


data2<-data[-c(outlierd1.index,outlierd2.index,outlierd3.index,outlierd5.index,
               outlierd6.index,outlierd7.index),]
summary(data2)
str(data2)

####################
####################

#Conversion en facteur des différents éléments
data2$year<-as.factor(data2$year)
data2$month<-as.factor(data2$month)
data2$month_abs_num<-as.factor(data2$month_abs_num)
data2$quarter<-as.factor(data2$quarter)


#Classification des différents ratios, temps de conversation et temps de travail
##################
######################
######################################################
summary(data2)


summary(data2$ratio.appels.entrants)

summary(data2$ratiowrapup)
#Moins de 15%, entre 15 et 25%, 25% et plus pour les ratios de wrapup
summary(data2$ratiowrapup)
data2$ratiowrapup.quali<-cut(data2$ratiowrapup,c(0,0.15,0.25,1),
                             include.lowest = TRUE, 
                             labels = c("Moins de 15% de taux de retrait","Entre 15% et 
                                         25% de taux de retrait", "Plus de 25% de taux 
                                        de retrait"))
summary(data2$ratiowrapup.quali)


#Moins de 50%, entre 50 et 65% et plus de 65% pour ratio entrants
summary(data2$ratio.appels.entrants)
data2$ratio.appels.entrants.quali<-cut(data2$ratio.appels.entrants,c(0,0.50,0.65,1),
                                       include.lowest = TRUE,
                                       labels=c("Moins de 50% d'appels entrants","Entre
                                                50% et 65% d'appels entrants",
                                                "Plus de 65% d'appels entrants"))
summary(data2$ratio.appels.entrants.quali)

#Moins de 90%, plus de 90% pour les taux de décroché
summary(data2$taux.de.decroché)
data2$taux.de.decroché.quali<-cut(data2$taux.de.decroché,c(0,0.90,1),
                                  include.lowest=TRUE, 
                                  labels=c("Moins de 90% de taux de prise d'appels ",
                                           "Plus de 90% de taux de prise d'appels"))
summary(data2$taux.de.decroché.quali)


#Moins de 5% d'appels sortants , entre 5% et 10% et plus de 10% (ratio appels sortants)
summary(data2$ratio.appels.sortants)
data2$ratio.appels.sortants.quali<-cut(data2$ratio.appels.sortants,
                                        c(0,0.05,0.1,1), include.lowest=TRUE,
                                        labels=c("Moins de 5% d'appels sortants",
                                                 "Entre 5% et 10% d'appels sortants",
                                                 "Plus de 10% d'appels sortants"))
summary(data2$ratio.appels.sortants.quali)

#Moins de 1 min, entre 1 et 2 min et plus de 2 min (temps moyen appels sortants)
summary(data2$temps.moyen.de.conversation.sur.appels.sortants)


data2$temps.moyen.de.conversation.sur.appels.sortants.quali<-cut(data2$temps.moyen.de.conversation.sur.appels.sortants,
                                                           c(0,1,2,21), include.lowest = TRUE,
                                                           labels=c("Moins de 1 min moyen sortants",
                                                                    "Entre 1 et 2 min moyen sortants", 
                                                                    "Plus de 2 min moyen sortants"))

summary(data2$temps.moyen.de.conversation.sur.appels.sortants.quali)


#On peut éventuellement soit garder toutes les observations soit supprimer les valeurs étant
#supérieures à 15 min. Dans ce cas précis, nous décidons donc de les garder pour tracer nos
#différentes classes

#Moins de 2 min, entre 2 et 3 min et plus de 3 min (temps moyen appels entrants)
summary(data2$temps.moyen.de.conversation.sur.appels.entrants)
data2$temps.moyen.de.conversation.sur.appels.entrants.quali<-cut(data2$temps.moyen.de.conversation.sur.appels.entrants,
                                                                 c(0,2,3,7), include.lowest = TRUE,
                                                                 labels=c("Moins de 2 min moyen entrants",
                                                                          "Entre 2 et 3 min moyen entrants", 
                                                                          "Plus de 3 min moyen entrants"))
summary(data2$temps.moyen.de.conversation.sur.appels.entrants.quali)
data2<-na.omit(data2)

#Catégorisation des variables temps total de conversation par jour
summary(data2$durée.appels.sortants.jour)
#En prenant soin de convertir la variabe sur le temps appels sortants jour en heure
data2$durée.appels.sortants.jour<- (data2$durée.appels.sortants.jour * 60)
summary(data2$durée.appels.sortants.jour)
d9<-boxplot(data2$durée.appels.sortants.jour, main="Boxplot sur la durée des appels sortants
            par jour")
outlier.d9<-boxplot.stats(data2$durée.appels.sortants.jour)$out
data2$durée.appels.sortants.jour.quali<-cut(data2$durée.appels.sortants.jour,
                                            c(0,15,25,40,50),include.lowest=TRUE,
                                            labels=c("Moins de 15 min d'appels sortants",
                                                     "Entre 15 et 25 min d'appels sortants",
                                                     "Entre 25 et 40 min d'appels sortants",
                                                    "Plus de 40 min d'appels sortants"))
summary(data2$durée.appels.sortants.jour.quali)
#Pour les appels entrants
summary(data2$durée.appels.entrants.jour)
data2$durée.appels.entrants.jour.quali<-cut(data2$durée.appels.entrants.jour,
                                            c(0,3,4,6), include.lowest=TRUE,
                                            labels=c("Moins de 3H d'appels entrants",
                                                     "Entre 3 et 4H d'appels entrants",
                                                     "Plus de 4H d'appels entrants"))
summary(data2$durée.appels.entrants.jour.quali)

#data2$nombre.d.appels.pris.par.heure.de.disponibilité.quali<-cut(data2$nombre.d.appels.pris.par.heure.de.disponibilité,
                                                                 #c(0,18,22,30),include.lowest=TRUE,
                                                                 #labels=c("Moins de 18 appels par heure",
                                                                          #"Entre 18 et 22 appels par heure",
                                                                          #"Plus de 22 appels par heure"))

#Ajouter les variables temps plein ainsi que les variables métiers
#Eventuellement songer à supprimer les zéros ainsi que les variables nombre pour observer le 
#résultat
summary(data2)
data2<-na.omit(data2)

