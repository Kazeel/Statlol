#############################################################################
#Functions LOL
#############################################################################
# You need this packages :

# library(jsonlite)
# library(curl)
# library(httr)
############################################################################
# Functions using Riot APi
#############################################################################
# lol.player : get summoner info with a summonername
# lol.matchslist.r : get ranked game list of a player with a account id
# lol.matches : get game info with game id

######################################################
#lol.player
######################################################
# get summoner info with a summonername
# see Riot Api for more information
# pseudo : summonername of a player (no space)
# serveur : region
# key : a Riot Api key
########################################################

lol.player <- function(pseudo, serveur, key){
  
  pseudo<- tolower(pseudo)
  pseudo<- gsub(" ", "", pseudo, fixed = TRUE)
  fichier.json<-paste("https://",serveur,".api.riotgames.com/lol/summoner/v3/summoners/by-name/",pseudo,"?api_key=",key,sep="")
  liste<- fromJSON(fichier.json)
  return(liste)
  
}

#######################################################
# lol.matchslist.r
#####################################################
# get ranked matchslist of a player
# see Riot Api for more information
# account.id : account id of a player
# serveur : region
# key : a Riot Api key
########################################################

lol.matchslist.r <- function(account.id, serveur, key){
  
  fichier.json<-paste("https://",serveur,".api.riotgames.com/lol/match/v3/matchlists/by-account/",account.id,"?queue=440&api_key=",key,sep="")
  liste<- fromJSON(fichier.json)
  return(liste)
  
}

#######################################################
# lol.matches
#####################################################
# get matches info of a game
# see Riot Api for more information
# game.id : game id of a game
# serveur : region
# key : a Riot Api key
########################################################

lol.matches <- function(game.id, serveur, key){
  
  fichier.json<-paste("https://",serveur,".api.riotgames.com/lol/match/v3/matches/",game.id,"?api_key=",key,sep="")
  liste<- fromJSON(fichier.json)
  return(liste)
  
}

#############################################################