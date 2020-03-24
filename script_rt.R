library(ggplot2)
library(dplyr)
library(tidyverse)
library(ade4)
library(corrplot)
library(Hmisc)
library(gridExtra)
library(lubridate)
################################
################################
data<-read.csv2(file ="source_traitée.csv", header=TRUE,sep=";",dec=",")
nrow(data)
# Rows with NAs
data.NAs <- data[rowSums(is.na(data)) > 0,]
# Number of rows with at least one NA
nrow(data.NAs)
# Count NAs by column
as.matrix(
  sapply(data.NAs, function(x) sum(is.na(x)) )
)
# Keep only rows with no NA
data<-na.omit(data)

str(data)
#Convertissons les durées en minutes et les durées totales en heures

#Temps total de disponibilité
data$temps.total.de.disponibilité<-hms(data$temps.total.de.disponibilité)
data$temps.total.de.disponibilité<-(
  hour(data$temps.total.de.disponibilité)+
    (minute(data$temps.total.de.disponibilité)/60)+
    (second(data$temps.total.de.disponibilité)/3600))
summary(data$temps.total.de.disponibilité)

#Temps total de conversation sur appels entrants
data$temps.total.de.conversation.sur.appels.entrants<-hms(data$temps.total.de.conversation.sur.appels.entrants)
data$temps.total.de.conversation.sur.appels.entrants<-(
  hour(data$temps.total.de.conversation.sur.appels.entrants)+
    (minute(data$temps.total.de.conversation.sur.appels.entrants)/60)+
    (second(data$temps.total.de.conversation.sur.appels.entrants)/3600))
summary(data$temps.total.de.conversation.sur.appels.entrants)

#Temps moyen de conversation sur appels entrants
data$temps.moyen.de.conversation.sur.appels.entrants<-hms(data$temps.moyen.de.conversation.sur.appels.entrants)
data$temps.moyen.de.conversation.sur.appels.entrants<-(
  hour(data$temps.moyen.de.conversation.sur.appels.entrants)*60+
    (minute(data$temps.moyen.de.conversation.sur.appels.entrants))+
    (second(data$temps.moyen.de.conversation.sur.appels.entrants)/60))

#Temps total de conversation sur appels sortants
data$temps.total.conversation.sur.appels.sortants<-hms(data$temps.total.conversation.sur.appels.sortants)
data$temps.total.conversation.sur.appels.sortants<-(
  hour(data$temps.total.conversation.sur.appels.sortants)+
    (minute(data$temps.total.conversation.sur.appels.sortants)/60)+
    (second(data$temps.total.conversation.sur.appels.sortants)/3600))

#Temps total de conversation
data$temps.total.de.conversation<-hms(data$temps.total.de.conversation)
data$temps.total.de.conversation<-(
  hour(data$temps.total.de.conversation)+
    (minute(data$temps.total.de.conversation)/60)+
    (second(data$temps.total.de.conversation)/3600))

#Temps total de retrait
data$temps.total.de.retrait<-hms(data$temps.total.de.retrait)
data$temps.total.de.retrait<-(
  hour(data$temps.total.de.retrait)+
    (minute(data$temps.total.de.retrait)/60)+
    (second(data$temps.total.de.retrait)/3600))
#Ajout de la variable temps total de travail
data$temps.total.de.travail<-0
data$temps.total.de.travail<-(data$temps.total.de.disponibilité + data$temps.total.de.retrait)
summary(data$temps.total.de.travail)

#Ajout de la variable temps moyen de conversation sur appels sortants
data$temps.moyen.de.conversation.sur.appels.sortants<-((data$temps.total.conversation.sur.appels.sortants/
                                                         data$nb.appels.sortants)*60)

describe(data)

#Ajout de la variable temps total de travail sur une journée
#En faisant l'hyporhèse que nous avons en général 20 jours travaillés par mois
data$temps.total.de.travail.jour<-(data$temps.total.de.travail/20)
                                         
summary(data$temps.total.de.travail.jour)

#Ajout de ratios permettant de mesurer la performance individuelle des agents et leurs comportements
data$ratiowrapup<-ifelse(data$temps.total.de.retrait==0,0,
                         data$temps.total.de.retrait/data$temps.total.de.travail)
data$ratio.appels.entrants<-ifelse(data$temps.total.de.travail==0,0,
                                   data$temps.total.de.conversation.sur.appels.entrants/data$temps.total.de.travail)

data$ratio.appels.sortants<-ifelse(data$temps.total.de.travail==0,
                                   0,data$temps.total.conversation.sur.appels.sortants/data$temps.total.de.travail)

data$taux.de.decroché<-ifelse(data$Appels.présentés==0,0, data$Appels.pris/data$Appels.présentés)

data$durée.appels.sortants.jour<-(data$ratio.appels.sortants * data$temps.total.de.travail.jour)
data$durée.appels.entrants.jour<-(data$ratio.appels.entrants * data$temps.total.de.travail.jour)

#Statistiques descriptives sur chacune des variables
summary(data$durée.appels.entrants.jour)
summary(data$temps.total.de.travail.jour) 

summary(data$ratio.appels.entrants)

summary(data$Nombre.d.études.AMO.AFF.MTL)
summary(data$AMO.NON.AFF.MTL.Nombre.d.études.AMO)
which(data$AMO.NON.AFF.MTL.Nombre.d.études.AMO == max(data$AMO.NON.AFF.MTL.Nombre.d.études.AMO))
#De bonnes pratiques reviendraient à pousser les individus à faire beaucoup plus d'études en ligne
summary(data$FL.MTL.Nombre.d.études.pour.ouvertures)

#Si on veut creuser un peu plus pour l'analyse des classes, on pourrait par exemple
#essayer de catégoriser l'ensemble des nombres d'études afin de pouvoir orienter la 
#prise de décision des agents

