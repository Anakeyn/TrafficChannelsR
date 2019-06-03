##########################################################################
# Auteur : Pierre Rouarch 2019 - Licence GPL 3
# TrafficChannelsR
# Typologie du trafic - Canaux.
# Pour illustrer notre propos nous utiliserons le jeu de données de 
# l'association Networking-Morbihan 
##########################################################################
#Packages et bibliothèques utiles (décommenter au besoin)
##########################################################################
#install.packages("lubridate")  #si vous ne l'avez pas
#install.packages("tseries")
#install.packages("devtools")
#devtools::install_github("twitter/AnomalyDetection")  #pour anomalyDetection de Twitter
#install.packages("XML")
#install.packages("stringi")
#install.packages("BSDA")
#install.packages("BBmisc")
#install.packages("stringi")
#install.packages("FactoMineR")
#install.packages("factoextra")
#install.packages("rcorr")

#install.packages("lubridate")  #si vous ne l'avez pas
library (lubridate) #pour yday
#library(tseries) #pour ts
library(AnomalyDetection) #pour anomalydetectionVec
#library(XML) # pour xmlParse
#library(stringi) #pour stri_replace_all_fixed(x, " ", "")
library(BSDA)  #pour SIGN.test 
library(BBmisc) #pour which.first
#install.packages("stringi")
library(stringi) #pour stri_detect
#library(ggfortify)  #pour ploter autoplot type ggplot
#install.packages("tidyverse")  #si vous ne l'avez pas #pour gggplot2, dplyr, tidyr, readr, purr, tibble, stringr, forcats 
#install.packages("forecast") #pour ma
#Chargement des bibliothèques utiles
library(tidyverse) #pour gggplot2, dplyr, tidyr, readr, purr, tibble, stringr, forcats 
library(forecast)  #pour  arima, ma, tsclean

##########################################################################
# Récupération du Jeu de données nettoyé des pages et des articles
##########################################################################
dfPageViews <- read.csv("dfPageViews.csv", header=TRUE, sep=";") 
#str(dfPageViews) #verif
#transformation de la date en date :-)
dfPageViews$date <- as.Date(dfPageViews$date,format="%Y-%m-%d")
#str(dfPageViews) #verif
str(dfPageViews) #72821 obs
dfPageViews$index <- 1:nrow(dfPageViews)  #création d'un pour retrouver les "articles marketing" 
#ensuite
#pour les articles
myArticles <- read.csv("myArticles.csv", header=TRUE, sep=";") 
#transformation de la date en date :-)
myArticles$date <- as.Date(myArticles$date,format="%Y-%m-%d")
#str(myArticles) #verif


############################################################
# Typologie du trafic entrant 
############################################################
#Revenons au Dataframe des pages vues dfPageViews
str(dfPageViews)
#regardons ce que l'on a dans la variable medium.
plyr::count(as.factor(dfPageViews$medium))
#La variable medium ne nous donne pas une information fiable.
#Remarque "(none)" devrait indiquer du trafic direct i.e 
#la personne a indiqué l'url dans la barre de menu de son 
# navigateur (ce qui arrive pratiquement jamais aujourd'hui). 
# Dans les faits il s'agit de sources non repérées par Google 
#et le plus souvent des clients emails ou des robots (encore !!)

#il serait intéressant de dissocier les canaux :
# - le trafic suite à un email  :  email en fait nous ne pouvons 
#   repérer que le Webmail dans source
# - le trafic via un site quelqonque : referral
# - le trafic via les réseaux sociaux : social
# - le trafic via des moteurs de recherche. : search
# - le trafic direct : direct (un peu fourre tout et contient de 
#   l'email aussi ... :-( )


#regardons les différentes sources
(mySources <- plyr::count(as.factor(dfPageViews$source)))
names(mySources)[1]<- "source"
str(mySources)

#Sauvegarde pour travailler à la main le type de sources.
write.csv2(mySources, file = "mySources.csv",  row.names=FALSE)
#.... traitement manuel externe .... 

#recuperer le fichier avec les channels 
mySourcesChannel <- read.csv2("mySourcesChannel.csv", header=TRUE)
str(mySourcesChannel) #voyons ce que l'on récupère
#pour effectuer le left join besoin d'une chaine de caractère.
mySourcesChannel$source <- as.character(mySourcesChannel$source)

##########################################################################
# Pour le traffic Global
##########################################################################
#recuperation de la variable channel dans le dataframe 
#principal par un left join.
dfPVChannel <- left_join(dfPageViews, mySourcesChannel, by="source")
#verifs
str(dfPVChannel)
head(dfPVChannel)
(myChannels <- plyr::count(as.factor(dfPVChannel$channel)))

#plot(myChannels)

#creation de la dataframe dateChannel_data  par jour et canal

dateChannel_data <- dfPVChannel[,c("date", "channel")]  %>%  #dateChannel_data  à partir de dfPVChannel
  group_by(date, channel) %>%                              #groupement par date et channel
  mutate(pageViews = n()) %>%              #total des pageviews = nombre d'observations / date /channel
  as.data.frame() %>%                             #sur d'avoir une data.frame
  mutate(year = format(date,"%Y"))             #creation de la variable year
                

str(dateChannel_data)
head(dateChannel_data)
#View(dateChannel_data)



##########################################################################
# Graphique en barre général Répartition du trafic selon les canaux.
ggplot() + 
  geom_bar(data=dateChannel_data, aes(x=channel, fill=channel)) +
  theme(axis.text.x = element_blank()) +
  ylab("Pages vues") +
  labs(title = "Le canal 'search' est le premier contributeur en termes de trafic.", 
       subtitle = "Le canal 'direct' (fourre tout) est malheureusement important aussi", 
       caption = "Trafic Global - Pages vues selon les canaux depuis 2011")
ggsave(filename = "PV-Channel-bar.jpg",  dpi="print") 

#Graphique en barre par année
#Répartition du trafic selon les sources.
ggplot() + 
  geom_bar(data=dateChannel_data, aes(x=channel, fill=channel)) +
  facet_wrap(~year, scales="free") +
  theme(axis.text.x = element_blank()) +
  ylab("Pages vues") +
  labs(title = "Le canal 'social' avait une forte contribution relative en 2011.", 
       subtitle = "La répartition des autres canaux est restée relativement stable.", 
       caption = "Trafic Global - Pages vues selon les années et les canaux depuis 2011")
ggsave(filename = "PV-Channel-bar-an.jpg",  dpi="print") 

#evolution des pages vues selon les canaux 
ggplot() + 
  geom_smooth(data=unique(dateChannel_data), aes(x=date, y=pageViews, col = channel)) +
  ylab("Pages vues") +
  labs(title = "Le canal 'search' a augmenté jusqu'en 2015 puis a baissé fortement.", 
       subtitle = "Les autres canaux ont régulièrement baissé", 
       caption = "Trafic Global - Evolution lissée des pages vues selon les canaux depuis 2011")
ggsave(filename = "PV-Channel-smooth.jpg",  dpi="print") 



#boxplot  selon les canaux - non repris dans le dossier
ggplot() + 
  geom_boxplot(data=unique(dateChannel_data), aes(x=channel, y=pageViews, fill=channel))
ggsave(filename = "PV-Channel-boxplot.jpg",  dpi="print") 

#boxplot selon les canaux et les années - non repris dans le dossier
ggplot() + 
  geom_boxplot(data=unique(dateChannel_data), aes(x=channel, y=pageViews, fill=channel)) +
  theme(axis.text.x = element_blank()) +
  facet_wrap(~year, scales="free")
ggsave(filename = "PV-Channel-year-boxplot.jpg",  dpi="print") 



#sauvegarde éventuelle pour SAS.
write.csv2(dateChannel_data, file = "dateChannel_data.csv",  row.names=FALSE)

##########################################################################
# Pour le traffic de base
##########################################################################
str(dfBasePageViews) #37615 obs.
#recuperation de la variable channel dans la dataframe principale par un left join.
dfBasePVChannel <- left_join(dfBasePageViews, mySourcesChannel, by="source")
#verifs
str(dfBasePVChannel)
head(dfBasePVChannel)
plyr::count(as.factor(dfBasePVChannel$channel))


#creation de la dataframe dateChannel_data  par jour et canal pour le 
# trafic de base

dateChannel_baseData <- dfBasePVChannel[,c("date", "channel")]  %>%  #dateChannel_basedata  à partir de dfPVChannel
  group_by(date, channel) %>%                              #groupement par date et channel
  mutate(pageViews = n()) %>%              #total des pageviews = nombre d'observations / date /channel
  as.data.frame() %>%                             #sur d'avoir une data.frame
  mutate(year = format(date,"%Y"))             #creation de la variable year

#verifs
str(dateChannel_baseData)
head(dateChannel_baseData)

##########################################################################
# Graphique en barre général Répartition du trafic selon
#les canaux pour le trafic de base
ggplot() + 
  geom_bar(data=dateChannel_baseData, aes(x=channel, fill=channel)) +
  theme(axis.text.x = element_blank()) +
  ylab("Pages vues") +
  labs(title = "Le canal 'search' est le premier contributeur en termes de trafic.", 
       subtitle = "Le canal 'direct' (fourre tout) est malheureusement important aussi ici", 
       caption = "Trafic de Base - Pages vues selon les canaux depuis 2011")
ggsave(filename = "Base-PV-Channel-bar.jpg",  dpi="print") 

#Graphique en barre par année
#Répartition du trafic de base selon les sources.
ggplot() + 
  geom_bar(data=dateChannel_baseData, aes(x=channel, fill=channel)) +
  facet_wrap(~year, scales="free") +
  theme(axis.text.x = element_blank()) +
  ylab("Pages vues") +
  labs(title = "La contribution relative du canal 'search' reste forte tous les ans.", 
       subtitle = "mais varie fortement en valeur absolue", 
       caption = "Trafic de Base - Pages vues selon les années et les canaux depuis 2011")
ggsave(filename = "Base-PV-Channel-bar-an.jpg",  dpi="print") 

#evolution des pages vues selon les canaux pour le trafic de base.
ggplot() + 
  geom_smooth(data=unique(dateChannel_baseData), aes(x=date, y=pageViews, col = channel)) +
  ylab("Pages vues") +
  labs(title = "Comme précédemment, Le canal 'search' a augmenté jusqu'en 2015 \n puis a baissé fortement.", 
       subtitle = "Les autres canaux ont régulièrement baissé, avec une légère reprise en 2018", 
       caption = "Trafic de Base - Evolution lissée des pages vues selon les canaux depuis 2011")
ggsave(filename = "Base-PV-Channel-smooth.jpg",  dpi="print") 

#sauvegarde éventuelle pour SAS.
write.csv2(dateChannel_baseData, file = "dateChannel_baseData.csv",  row.names=FALSE)



##########################################################################
#regardons pour le trafic Direct  Marketing uniquement i.e le traffic dont
# la source a dirigé vers une page Articles Marketing 

str(dfDMPageViews) #28553 
28553 / 37615
sum(dfDMPVChannel$channel=="search") / sum(dfBasePVChannel$channel=="search")
dfDMPVChannel <- left_join(dfDMPageViews, mySourcesChannel, by="source")
str(dfDMPVChannel)
#creation de la dataframe dateChannel_AMdata  par jour

dateChannel_DMData <- dfDMPVChannel[,c("date", "channel")]  %>%  #dateChannel_DMdata  à partir de dfPVChannel
  group_by(date, channel)  %>%   #on groupe par date et channel
  mutate(pageViews = n()) %>%              #total des pageviews = nombre d'observations / date /channel
  as.data.frame() %>%                             #sur d'avoir une data.frame
  mutate(year = format(date,"%Y"))             #creation de la variable year




##########################################################################
# Graphique en barre général Répartition du trafic selon
#les canaux pour le trafic Direct Marketing

ggplot() + 
  geom_bar(data=dateChannel_DMData, aes(x=channel, fill=channel)) +
  theme(axis.text.x = element_blank()) +
  ylab("Pages vues") +
  labs(title = "Comme précédemment, le canal Search est le premier contributeur \n en termes de trafic.", 
       subtitle = "Le canal 'direct' (fourre tout) est malheureusement important aussi", 
       caption = "Direct Marketing - Pages vues selon les sources de trafic depuis 2011")
ggsave(filename = "DM-PV-Channel-bar.jpg",  dpi="print") 


#Graphique en barre par année
#Répartition du trafic direct marketing selon les sources.
ggplot() + 
  geom_bar(data=dateChannel_DMData, aes(x=channel, fill=channel)) +
  facet_wrap(~year, scales="free") +
  theme(axis.text.x = element_blank()) +
  ylab("Pages vues") +
  labs(title = "La contribution relative du canal 'search' a augmenté de 2011 à 2013.", 
       subtitle = "puis s'est stabilisée", 
       caption = "Direct Marketing - Pages vues selon les années et les canaux depuis 2011")
ggsave(filename = "DM-PV-Channel-bar-an.jpg",  dpi="print") 

#evolution des pages vues selon les canaux pour le trafic direct marketing
ggplot() + 
  geom_smooth(data=unique(dateChannel_DMData), aes(x=date, y=pageViews, col = channel)) +
  ylab("Pages vues") +
  labs(title = "Ici les canaux 'direct' et 'social' étaient plus important que le 'search' \n dans les premières années.", 
       subtitle = "La forme de la courbe du canal search' ne semble pas avoir beaucoup varié \n par rapport au trafic de base ou global.", 
       caption = "Direct Marketing - Evolution lissée des pages vues selon les canaux depuis 2011")
ggsave(filename = "DM-PV-Channel-smooth.jpg",  dpi="print") 

#sauvegarde éventuelle pour SAS.
write.csv2(dateChannel_DMData, file = "dateChannel_DMData.csv",  row.names=FALSE)


##########################################################################
#  Comparatif DM vs BAse
#evolution des pages vues selon les canaux pour le trafic direct marketing
ggplot() + 
  geom_smooth(data=unique(dateChannel_baseData), aes(x=date, y=pageViews, col = channel), se = FALSE) +
  geom_smooth(data=unique(dateChannel_DMData), aes(x=date, y=pageViews, col = channel), linetype="dashed", se = FALSE) +
  ylab("Pages vues") +
  labs(title = "Le canal 'search' évolue de façon équivalente selon le type de pages \n mais avec beaucoup moins de trafic pour les pages Marketing", 
       subtitle = "Les formes des courbes 'social' et 'direct' diffèrent.", 
       caption = "Base (lignes pleines) vs Direct Marketing (pointillés) \n Evolution lissée des pages vues selon les canaux depuis 2011")
ggsave(filename = "Base-DM-PV-Channel-smooth.jpg",  dpi="print") 


#Comparatif des proportions
#proportions des différents trafic 
propDMBase <- nrow(dfDMPVChannel) / nrow(dfBasePVChannel) #0.76
myPropDMBase<- data.frame(channel = c("direct", "referral", "search", "social", "webmail" ),
                          proportion = c(0,0,0,0,0))
nrow(dfBasePVChannel) / nrow(dfDMPVChannel)
myPropDMBase[1, "proportion"] <- sum(dfDMPVChannel$channel=="direct") / sum(dfBasePVChannel$channel=="direct") #0.81
myPropDMBase[2, "proportion"] <- sum(dfDMPVChannel$channel=="referral") / sum(dfBasePVChannel$channel=="referral")  #0.29
myPropDMBase[3, "proportion"] <- sum(dfDMPVChannel$channel=="search") / sum(dfBasePVChannel$channel=="search") #0.66
myPropDMBase[4, "proportion"] <-sum(dfDMPVChannel$channel=="social") / sum(dfBasePVChannel$channel=="social") #1.25
sum(dfBasePVChannel$channel=="social") /  sum(dfDMPVChannel$channel=="social") #0,80
myPropDMBase[5, "proportion"] <- sum(dfDMPVChannel$channel=="webmail") / sum(dfBasePVChannel$channel=="webmail") #1.26
sum(dfBasePVChannel$channel=="webmail") /  sum(dfDMPVChannel$channel=="webmail") #0.79

ggplot() + 
  geom_point(data=myPropDMBase, aes(x=channel, y=proportion, col = channel), size=5) +
  geom_hline(yintercept = propDMBase, color= "red" ) +
  ylab("Proportion Direct Marketing / Base") +
  labs(title = "Le traffic Direct Marketing est composé en proportion plus importante \n de trafic direct, social et webmail", 
       subtitle = "Le trafic de base de referral et de search.", 
       caption = "Proportions Direct Marketing / Base selon les canaux - Total depuis 2011")
ggsave(filename = "Base-DM-PV-Channel-Prop.jpg",  dpi="print") 


#verification que les proportions sont statistiquement valides.
#trafic direct 
#H0 prop DM/base direct <= DM/base Total ,  
#H1 : prop DM/base direct  > DM/base Total (p.value << 0.05)
(resPropTestDirect <- prop.test(x= sum(dfDMPVChannel$channel=="direct"),  
                                n = sum(dfBasePVChannel$channel=="direct"), 
                                p=propDMBase, 
                                alternative = "greater"))
resPropTestDirect$p.value #1.678199e-44 << 0.05 : H1


#trafic referral
#H0 prop DM/base referral >= DM/base Total ,  
#H1 : prop DM/base referral  < DM/base Total (p.value << 0.05)
(resPropTestDirect <- prop.test(x= sum(dfDMPVChannel$channel=="referral"),  
                                n = sum(dfBasePVChannel$channel=="referral"), 
                                p=propDMBase, 
                                alternative = "less"))
resPropTestReferral$p.value #0 << 0.05 #H1

#trafic search
#H0 prop DM/base search >= DM/base Total ,  
#H1 : prop DM/base search  < DM/base Total (p.value << 0.05)
(resPropTestSearch <- prop.test(x= sum(dfDMPVChannel$channel=="referral"),  
                                n = sum(dfBasePVChannel$channel=="referral"), 
                                p=propDMBase, 
                                alternative = "less"))
resPropTestSearch$p.value #0 << 0.05#H1

##########################################################################
#trafic social #! le test est inversé car 
#sum(dfDMPVChannel$channel=="social") > 
#sum(dfBasePVChannel$channel=="social")
#Par ailleurs nrow(dfBasePVChannel) / nrow(dfDMPVChannel) = 1,31 > 1 
#donc on teste avec p=0.99
#H0 proportion base/DM social >= 0.99  ,  H1 : prop base/DM social < 0.99  
#(p.value << 0.05)
(resPropTestSocial <- prop.test(x= sum(dfBasePVChannel$channel=="social"),  
                                n = sum(dfDMPVChannel$channel=="social"), 
                                p=0.99, 
                                alternative  = "less"))
resPropTestSocial$p.value #0 << 0.05 : H1 <0.99 donc à fortiori < 1,31


#trafic webmail, comme précédemment le test est inversé car 
#sum(dfDMPVChannel$channel=="webmail") > 
#sum(dfBasePVChannel$channel=="webmail")
#Par ailleurs nrow(dfBasePVChannel) / nrow(dfDMPVChannel) = 1,31 > 1 
#donc on teste avec p=0.99
#H0 proportion base/DM webmail >= 0.99  ,  H1 : prop base/DM webmail < 0.99 
#(p.value << 0.05)
(resPropTestWebmail <- prop.test(x= sum(dfBasePVChannel$channel=="webmail"),  
                                n = sum(dfDMPVChannel$channel=="webmail"), 
                                p=0.99, 
                                alternative  = "less"))
resPropTestWebmail$p.value #0 << 0.05 : H1 <0.99 donc à fortiori < 1,31



##########################################################################
# ACP - Analyse en Composantes Principales pour le 
# trafic Direct Marketing - Chaque observation est une page 
##########################################################################


################### PREPARATION DES DONNEES ##############################
str(dfDMPVChannel)
#il faut nettoyer les landing pages. Normalement on doit en avoir 95 
#différentes que l'on va prendre dans myArticles
#voyons ce que l'on a 
plyr::count(as.factor(dfDMPVChannel$landingPagePath))
#nettoyage des urls en doublons ou curieuses
dfDMPVChannel$cleanLandingPagePath <- word(dfDMPVChannel$landingPagePath, sep=fixed("?"))
dfDMPVChannel$cleanLandingPagePath <- word(dfDMPVChannel$cleanLandingPagePath , sep=fixed("&"))
dfDMPVChannel$cleanLandingPagePath <- word(dfDMPVChannel$cleanLandingPagePath , sep=fixed("<"))
dfDMPVChannel$cleanLandingPagePath <- word(dfDMPVChannel$cleanLandingPagePath , sep=fixed("/ngg"))
dfDMPVChannel$cleanLandingPagePath <- word(dfDMPVChannel$cleanLandingPagePath , sep=fixed(")"))
plyr::count(as.factor(dfDMPVChannel$cleanLandingPagePath)) #il en reste encore ...
#on va nettoyer avec un inner_join 
str(dfDMPVChannel) #28553
str(myArticles) #95
myCleanLandingPagePath <- data.frame(myArticles[, "pagePath"])
colnames(myCleanLandingPagePath)[1] <- "cleanLandingPagePath" 
#besoin de caractères pour le inner_join
myCleanLandingPagePath$cleanLandingPagePath <- as.character(myCleanLandingPagePath$cleanLandingPagePath)
#clean par le join.
dfDMPVChannelClean <- inner_join(x=dfDMPVChannel, y=myCleanLandingPagePath, by="cleanLandingPagePath")
plyr::count(as.factor(dfDMPVChannelClean$cleanLandingPagePath)) #ok à 95 
str(dfDMPVChannelClean) #28481 obs  on en a perdu environ 80



LPPChannel_DMDataForACP <-  dfDMPVChannelClean[, c("cleanLandingPagePath", "channel")]  %>%   #on ne garde que cleanLandingPagePath et channel
  group_by(cleanLandingPagePath, channel) %>%   #groupement par cleanLandingPagePath et channel
  mutate(Pageviews = n()) %>%                   #on décompte les pages vues
  unique() %>%                                  #découblonnement
  spread(key=channel, value=Pageviews, fill = 0, convert = FALSE, drop = TRUE,
         sep = NULL)  #eclatement du facteur channel en variables 

#verifs
str(LPPChannel_DMDataForACP)
#plus clair : que en data.frame
LPPChannel_DMDataForACP <- as.data.frame(LPPChannel_DMDataForACP) #que data.frame
#ne sert pas à grand chose
rownames(LPPChannel_DMDataForACP) <- LPPChannel_DMDataForACP[,1]

#sauvegarde éventuelle pour SAS.
write.csv2(LPPChannel_DMDataForACP, file = "LPPChannel_DMDataForACP.csv",  row.names)

##########################################################################
# Exploration préalable 
##########################################################################
#Summary
summary(LPPChannel_DMDataForACP[,-1])
#plot de toutes les variables 2 à 2
plot(LPPChannel_DMDataForACP[, -1]) #pas très lisible - non utilisé
#on découpe 2 à 2 - pas beaucoup plus explicites: trop de points en dehors de l'IC. non utilisés
ggplot(data=LPPChannel_DMDataForACP, aes(x=direct, y=referral)) + geom_point() + geom_smooth(method = lm)
ggplot(data=LPPChannel_DMDataForACP, aes(x=direct, y=search)) + geom_point() + geom_smooth(method = lm)
ggplot(data=LPPChannel_DMDataForACP, aes(x=direct, y=social)) + geom_point() + geom_smooth(method = lm)
ggplot(data=LPPChannel_DMDataForACP, aes(x=direct, y=webmail)) + geom_point() + geom_smooth(method = lm)
ggplot(data=LPPChannel_DMDataForACP, aes(x=referral, y=search)) + geom_point() + geom_smooth(method = lm)
ggplot(data=LPPChannel_DMDataForACP, aes(x=referral, y=social)) + geom_point() + geom_smooth(method = lm)
ggplot(data=LPPChannel_DMDataForACP, aes(x=referral, y=webmail)) + geom_point() + geom_smooth(method = lm)
ggplot(data=LPPChannel_DMDataForACP, aes(x=search, y=social)) + geom_point() + geom_smooth(method = lm)
ggplot(data=LPPChannel_DMDataForACP, aes(x=search, y=webmail)) + geom_point() + geom_smooth(method = lm)
ggplot(data=LPPChannel_DMDataForACP, aes(x=social, y=webmail)) + geom_point() + geom_smooth(method = lm)


#tests de normalité et distributions
#direct
shapiro.test(LPPChannel_DMDataForACP$direct)
ggplot(data=LPPChannel_DMDataForACP, aes(x=direct)) +  geom_density(color="blue")
#referral
shapiro.test(LPPChannel_DMDataForACP$referral)
ggplot(data=LPPChannel_DMDataForACP, aes(x=referral)) +  geom_density(color="blue") 
#search
shapiro.test(LPPChannel_DMDataForACP$search)
ggplot(data=LPPChannel_DMDataForACP, aes(x=search)) +  geom_density(color="blue") +
  labs(title = "la variable search montre clairement que peu de pages \n concourent à beaucoup de trafic", 
       subtitle = "i.e : peu de pages sont bien référencées dans Google.", 
       caption = "Direct Marketing : Distribution du trafic 'search' selon les pages" )
  ggsave(filename = "DM-Distribution-PV-search.jpg",  dpi="print") 
#social
shapiro.test(LPPChannel_DMDataForACP$social)
ggplot(data=LPPChannel_DMDataForACP, aes(x=social)) +  geom_density(color="blue")
#webmail
shapiro.test(LPPChannel_DMDataForACP$webmail)
ggplot(data=LPPChannel_DMDataForACP, aes(x=webmail)) +  geom_density(color="blue")
#rem aucune distribution n'est normale 
 
#matrice de corrélation 

#matrice de correlation de Pearson (pour loi normale)
cor(LPPChannel_DMDataForACP[, -1], method = "pearson") 
#matrice de correlation de Kendall
cor(LPPChannel_DMDataForACP[, -1], method = "kendall") 
#matrice de correlation de Spearman 
cor(LPPChannel_DMDataForACP[, -1], method = "spearman") 

#cor avec  p.value Pearson
rcorr(as.matrix(LPPChannel_DMDataForACP[, -1]), type="pearson")  
#cor avec  p.value Spearman <- sélectionné 
rcorr(as.matrix(LPPChannel_DMDataForACP[, -1]), type="spearman") 

##########################################################################
# ACP (ici avec prcomp)
##########################################################################
#ACP avec prcomp
res.prcomp <- prcomp(LPPChannel_DMDataForACP[, -1])
str(res.prcomp) # voyonsce que l'on a
summary(res.prcomp)
#composantes principales
res.prcomp$rotation
#PC1         PC2          PC3          PC4         PC5
#direct   -0.17269362 -0.81251801 -0.535101162  0.002949279 -0.15378376
#referral -0.03906164 -0.05606092  0.066708334 -0.991449594  0.08893291
#search   -0.97596063  0.21595999 -0.008883743  0.024415395 -0.01367827
#social   -0.12257270 -0.52839746  0.834440292  0.093339157  0.02773560
#webmail  -0.03358394 -0.10406281 -0.113345590  0.087810562  0.98360822
#fraction d'information (sdev="standard deviation") 
res.prcomp$sdev
#[1] 229.97505  72.99133  39.20518  15.78361  11.27184

#proportion de variance de chaque composante
(percent_var_explained <- (res.prcomp$sdev^2 / sum(res.prcomp$sdev^2))*100)
#[1] 87.9577265  8.8604352  2.5562269  0.4143101  0.2113014
#Graphique "ScreePlot pourcentage de variance.
#il nous faut une dataframe
myPVE <- data.frame(
  PC=1:length(percent_var_explained),
  PVE=percent_var_explained
)
str(myPCV)
ggplot(data = myPVE, aes(x=PC, y=PVE)) + 
  geom_col() +
  xlab("Composantes") +
  ylab("% Variance") +
  labs(title = paste("La première composante comprend déja", round(myPVE$PVE[1],2), " % de l'information"), 
       subtitle = "", 
       caption = "Screeplot du % de variance des composantes de l'ACP \n pour les canaux Direct Marketing"  )
ggsave(filename = "DM-PCA-screeplot-channel.jpg",  dpi="print")

#Variance totale PC1+PC2 en %
sum(100 * (res.prcomp$sdev^2)[1:2] / sum(res.prcomp$sdev^2))
#les 2 premières composantes contiennent 96.81816 % de l'information
.

#nuage des individus et axes des variables
ggplot2::autoplot(res.prcomp, 
         data = LPPChannel_DMDataForACP,
         loadings = TRUE,
         loadings.label = TRUE, 
         loadings.label.size = 3) +
         labs(title = paste("search est quasiment dans l'axe de la composante 1"), 
              subtitle = "direct, webmail et social et un peu moins referral sont \n quasiment tous perpendiculaires à search", 
              caption = "Nuage des pages et axes des variables Direct Marketing")

ggsave(filename = "DM-PCA-cloud-channel.jpg",  dpi="print")

##########################################################################
# MERCI pour votre attention !
##########################################################################

