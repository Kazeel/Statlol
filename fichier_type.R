#############################
#Programme type
###########################
#
#
#
#
###########################
# Donn?es utilisateurs
##########################
#
#
#
############################

pseudo <- "Kazeel"
serveur <- "euw1"
saison <- 2016
key    <- "RGAPI-93834b4f-b9ab-4f0b-8d78-3a338cecf531"

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
account.id<-result.id["accountId"] #Get ID

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

######################################

dir.create("team_data")

#Test team.player
team.ids <- team.players(vec.joueurs,serveur,key)

#Save
team.folder <- paste("./team_data/",paste(team.ids, collapse = "_"), sep = "")
dir.create(team.folder) # Create a specific folder for a team
write.csv(team.ids, file = paste(team.folder,"/ids_joueurs.csv",sep = "")) #Enregistrement

#Test team.matchslist
team.matchs.list <- team.matchslist(vec.joueurs, team.ids, serveur, key)

#Save
write.csv(team.matchs.list, file = paste(team.folder,"/matchs_list.csv",sep = "")) #Enregistrement

#Test team.matchsstats
team.matchs.stats <- team.matchsstats(team.matchs.list, serveur, key)

#Save
write.csv(team.matchs.stats, file = paste(team.folder,"/matchs_stats.csv",sep = "")) #Enregistrement
