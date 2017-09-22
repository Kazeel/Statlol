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
      players.ids[i]<-lol.player(pseudo = vec.players[i], serveur = serveur, key = key)[["accountId"]]
    } else{
      players.ids[i] <- 0
    }
  }
  return(players.ids)
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
  data.matchs.team <- unique(data.matchs[,c(1,2,5,6)])
  return(data.matchs.team)
  
}


#######################################################
# team.matchsstats
#####################################################
# get all the stat of a list of games (for all players)
# see Riot Api for more information
# data.matchs.team : data.frame of a game id list (see team matchslist)
# serveur : region
# key : a Riot Api key
########################################################

team.matchsstats <- function(data.matchs.team, serveur, key){
  
  # Init
  vec.games.ids <- data.matchs.team$gameId
  loop.tab <- data.frame()
  
  json.tab <- NULL
  stat.tab <- NULL
  stats.stat.tab <- NULL
  
  participants.tab <- NULL
  player.partcipants.tab <- NULL
  
  loop.merge <- NULL
  loop.tab<- NULL
  
  for(i in 1:length(vec.games.ids)){
    id.loop <- vec.games.ids[i]
    if(id.loop != 0 ){
      json.tab <- lol.matches(id.loop, serveur, key)
      
      if(json.tab$gameDuration >= 300){
        
        stat.tab <- json.tab[[12]]
        stats.stat.tab <- stat.tab$stats
        stat.tab <- cbind(stat.tab[,c("participantId", "teamId")],stats.stat.tab)
        names(stat.tab)[1]<- "partcipantId"
        
        participants.tab <- json.tab[["participantIdentities"]]
        player.partcipants.tab <-participants.tab$player
        participants.tab <- cbind(participants.tab[,1], player.partcipants.tab)
        names(participants.tab)[1]<- names(stat.tab)[1]
        
        loop.merge <- merge(stat.tab, participants.tab)
        
        duration <- rep(json.tab$gameDuration, 10)
        loop.merge<- cbind(loop.merge, duration)
        
        if((i!=1)&(length(names(loop.merge))!=length(names(loop.tab)))){
          firstInhibitorAssist <- rep(NA, nrow(loop.merge))
          firstInhibitorKill <- rep(NA, nrow(loop.merge))
          loop.merge <- cbind(loop.merge, firstInhibitorKill)
          names(loop.merge)[85]<- "firstInhibitorKill"
          loop.merge <- cbind(loop.merge, firstInhibitorAssist)
          names(loop.merge)[86]<- "firstInhibitorAssist"
        }
        
        row.names(loop.merge) <- paste(id.loop, c(0:9), sep = "")
        
        loop.tab<- rbind(loop.tab,loop.merge)
        
        if(i%%8 == 0){
          Sys.sleep(10) #Tant qu'on a pas une clef normale
        }
      }
    } 
  }
  return(loop.tab)
}

#############################################################