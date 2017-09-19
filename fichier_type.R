#############################
#Programme type
###########################
#
#
#
#
###########################
# Données utilisateurs
##########################
#
#
#
############################

pseudo <- "Kazeel"
serveur <- "euw1"
saison <- 2016
key    <- "RGAPI-ea3a2952-91ac-48be-96c0-0bcd83f505a9"

##########################
# Fonctions
#########################
#
#
#############################
setwd("C:/Documents/GitHub/Statlol")      # Attention, ce chemin n'est pas relatif
source("fonctions_lol.R", local = TRUE)
require(jsonlite)
require(curl)
require(httr)
require(TriMatch)

#####################################
#Algrotihme
####################################
#
#
#####################################

#récupérer l'ID
result.id<-  lol.idjoueur(pseudo, serveur, key)
account.id<-result.id[[2]]

#Récupérer la liste des matchs classés d'un joueur
result.matchslist <- lol.matchslist.ranked.easy(account.id, serveur, key)
matchs.list <- result.matchslist[[1]]
matchs.id.test <- 3347747326 #matchs.list[1,2]

#Récupérer les informations principales d'un matchs
result.matches <- lol.matches(matchs.id.test, serveur, key)
teams.data.test <- result.matches[[11]]
participants.data.test <- result.matches[[12]]
participants.id.data.test <- result.matches[["participantIdentities"]]
test.merge <- merge(participants.data.test, participants.id.data.test)#RechercheV

#Test Adresse des images et version

version<-lol.staticdata.version(server,key)[1]
square<-lol.staticdata.image("Urgot","square",0,version)
loading<-lol.staticdata.image("Urgot","loading",1,version)
