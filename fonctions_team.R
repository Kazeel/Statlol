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
# Return a vector with ids
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
  listes_matchs <- data.frame()
  nplayer <- 0
  for(i in 1:5){
    # Si l'Id est 0 alors vous n'avez pas rentr? un pseudo
    id.loop <- players.id[i]
    pseudo.loop <- vec.players[i]
    if(id.loop != 0 ){
      loop.list <- lol.matchslist.r(id.loop, serveur, key)[[1]]
      listes_matchs <- rbind(listes_matchs, loop.list)
      nplayer <- nplayer +1
    } 
  }
  table.list <- table(listes_matchs$gameId)
  table.list <- table.list[table.list %in% nplayer]
  team.games <- unique(listes_matchs[,c(1,2,5,6)])
  team.games <- team.games[team.games$gameId %in% names(table.list),]
  return(team.games)
  
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
      
      if(json.tab$gameDuration >= 300){#delete remake
        
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

#######################################################
# team.allstats
#####################################################
# get all the stat of a list of games (for all players)
# see Riot Api for more information
# vec.players : vector of player
# serveur : region
# key : a Riot Api key
########################################################

team.allstats<- function(vec.players, serveur, key){
  vec.id <- team.players(vec.players, serveur, key)
  data.matchs.team <- team.matchslist(vec.players, vec.id, serveur, key)
  data <- team.matchsstats(data.matchs.team, serveur, key)
  
  #rename (maybe a player rename)
  
  for(i in 1:length(vec.id)){
    data$summonerName[data$accountId == vec.id[i]]<-vec.players[i]
  }
  
  result <- team.cleaner(data, vec.id)
  return(result)
}

#######################################################
# team.cleaner
#####################################################
# clean data of a team (keep only a few data)
# data : data frame of a team
########################################################

team.cleaner <- function(data,vec.id){
  
  matchinfo <- data[data[,"accountId"] %in% vec.id,] #Filter : players of the team
  
  clean.column<-data.frame(
    #######################################
    # player
    Id=row.names(matchinfo),
    Name=matchinfo$summonerName,
    Id_Sum=matchinfo$summonerId,
    Id_Ac=matchinfo$accountId,
    
    #######################################
    # Stat player
    Kill=matchinfo$kills,
    Death=matchinfo$deaths,
    Assist=matchinfo$assists,
    
    T_Damage_D=matchinfo$totalDamageDealt,
    Damage_D=matchinfo$totalDamageDealtToChampions,
    Damage_Turret=matchinfo$damageDealtToTurrets,
    Turret=matchinfo$turretKills,
    Inhib=matchinfo$inhibitorKills,
    
    Heal=matchinfo$totalHeal,
    Heal_N=matchinfo$totalUnitsHealed,
    
    Score_Vision=matchinfo$visionScore,
    vison_ward=matchinfo$visionWardsBoughtInGame,
    ward=matchinfo$wardsPlaced,
    
    Damage_T=matchinfo$totalDamageTaken,
    CC_score=matchinfo$timeCCingOthers,
    CC_time=matchinfo$totalTimeCrowdControlDealt,
    
    Gold_E=matchinfo$goldEarned,
    Minions=matchinfo$totalMinionsKilled,
    Minionsbis=matchinfo$neutralMinionsKilled,
    
    F_Blood=matchinfo$firstBloodKill,
    F_Tower=matchinfo$firstTowerKill,
    ######################################
    # team
    Blue_Side=matchinfo$teamId,
    Win=matchinfo$win,
    Duree=matchinfo$duration,
    Id_game=substr(row.names(matchinfo),1,10))
  
  ############################################
  # Convertion
  
  clean.column$Id_game<-as.factor(clean.column$Id_game)
  clean.column$Blue_Side[clean.column$Blue_Side == 100]<-TRUE
  clean.column$Blue_Side[clean.column$Blue_Side == 200]<-FALSE

  return(clean.column)
}

#######################################################
# team.summary
#######################################################
# Give some (calculate) stat of the team (winrate etc...)
# data : a clean data of a team (use team.cleaner)
#######################################################

team.summary <-function(data,func){
  
  filter.data <- data[,c("Kill",
                         "Death",
                         "Assist",
                         "T_Damage_D",
                         "Damage_D",
                         "Damage_Turret",
                         "Turret",
                         "Inhib",
                         "Heal",
                         "Heal_N",
                         "Score_Vision",
                         "vison_ward",
                         "ward",
                         "Damage_T",
                         "CC_score",
                         "CC_time",
                         "Gold_E",
                         "Minions",
                         "Minionsbis",
                         "F_Blood",
                         "F_Tower",
                         "Blue_Side",
                         "Win",
                         "Duree"
                         )]
  result<-aggregate(filter.data,by=list(data$Name), FUN = func)
  
  return(result)
}