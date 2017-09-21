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
key    <- "RGAPI-e207fef8-d466-4103-9bf6-00c0ea48e0fe"

pseudo_top <- "RKSReidoz"
pseudo_jun <- "Kazeel"
pseudo_mid <- "Mashu"
pseudo_adc <- "RedWhale"
pseudo_sup <- ""

##########################
# Fonctions
#########################
#
#
#############################
setwd("C:/Documents/GitHub/Statlol")      # Attention, ce chemin n'est pas relatif
source("fonctions_lol.R", local = TRUE)
source("fonctions_team.R", local = TRUE)
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

vec.joueurs <- c("TOP"= pseudo_top, "JUN"= pseudo_jun, "MID"=pseudo_mid, "ADC"=pseudo_adc,"SUP"=pseudo_sup)

#Test lol.player
result.id<-  lol.player(pseudo, serveur, key)
account.id<-result.id[[2]] #Get ID

#Test lol.matchslist.r
result.matchslist <- lol.matchslist.r(account.id, serveur, key)
matchs.list <- result.matchslist[[1]] 
matchs.id.test <- matchs.list[1,2]

#Test lol.matches
result.matches <- lol.matches(matchs.id.test, serveur, key)
teams.data.test <- result.matches[[11]]
participants.data.test <- result.matches[[12]]
participants.id.data.test <- result.matches[["participantIdentities"]]
test.merge <- merge(participants.data.test, participants.id.data.test)#RechercheV

#Test team.player
team.ids <- team.players(vec.joueurs,serveur,key)

#Test team.matchslist
team.matchs.list <- team.matchslist(vec.joueurs, team.ids, serveur, key)

#Test team.matchsstats
team.matchs.stats <- team.matchsstats(team.matchs.list, serveur, key)
