#############################################################################
#Functions TEAM
#############################################################################
# You need this packages :

# library(jsonlite)
# library(curl)
# library(httr)
############################################################################
# Functions using fonctions_lol.R 
#############################################################################
# 
# 
# 

######################################################
#team.players
######################################################
# get summoners account ids of a team of 5 players
# Return a data frame with Name and id
# see Riot Api for more information
# vec.players : vector of summoners name
# serveur : region
# key : a Riot Api key
########################################################

team.players <- function(vec.players, serveur, key){
  # vec.players : vector of summoners name
  
  # Init
  len <- length(vec.players)
  players.ids <- c()
  for(i in 1:len){
    # Return 0 if a player is missing
    if(vec.players[i] != "" ){
      players.ids[i]<-lol.idjoueur(pseudo = vec.players[i], serveur = serveur, key = key)[["accountId"]]
    } else{
      players.ids[i] <- 0
    }
  }
  return(players.id)
}

#######################################################
# team.matchslist
#####################################################
# get ranked matchslist of team
# see Riot Api for more information
# vec.players : vector of summonerName
# players.id : vector of summoner account id
# serveur : region
# key : a Riot Api key
########################################################

team.matchslist <- function(vec.players, players.id, serveur, key){
  # 
  
  # Init
  len <- length(vec.players)
  data.matchs <- data.frame()
  
  # Algo
  for(i in 1:len){
    # Si l'Id est 0 alors vous n'avez pas rentré un pseudo
    id.loop <- players.id[i]
    pseudo.loop <- vec.players[i]
    if(id.loop != 0 ){
      data.matchs<- rbind(data.matchs,lol.matchslist.r(id.loop, serveur, key)[[1]])
    } 
  }
  data.matchs.team <- unique(flex.data.matchs[,c(1,2,5,6)])
  return(data.matchs.team)
  
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