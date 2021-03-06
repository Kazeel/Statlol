#############################
# Algorithme d'extraction des matchs d'une ?quipe
###########################
#
#
#
#
###########################
# Donn?es utilisateurs & sourcing
##########################
#
#
#
############################
library(googleVis)

serveur <- "euw1"
key    <- "RGAPI-c6db6ff7-f11b-4141-86b3-6d10c1842a76"

############################
# Donn?es joueurs
############################
# Penser ? retirer les espaces
#
#
############################

pseudo_top <- "RKS Reidoz"
pseudo_jun <- "Kazeel"
pseudo_mid <- "Mashu"
pseudo_adc <- "RedWhale"
pseudo_sup <- ""

###########################
# Generation de la liste des joueurs et de leurs account.id
###########################
#
#
#
###############################

pseudos_joueurs <- c("TOP"= pseudo_top, "JUN"= pseudo_jun, "MID"=pseudo_mid, "ADC"=pseudo_adc,"SUP"=pseudo_sup)
ids_joueurs <- c()
for(i in 1:5){
  # Si l'Id est 0 alors vous n'avez pas rentre un pseudo
  if(pseudos_joueurs[i] != "" ){
    ids_joueurs[i]<-lol.player(pseudo = pseudos_joueurs[i], serveur = serveur, key = key)[["accountId"]]
  } else{
    ids_joueurs[i] <- 0
  }
}

data.pseudo_id <- data.frame(pseudos_joueurs, ids_joueurs)

write.csv(data.pseudo_id, file = "ids_joueurs.csv") #Enregistrement

###########################
# R?cup?ration de la liste des matchs de chaque joueur
###########################
#
#
###############################

listes_matchs <- data.frame()
nplayer <- 0
for(i in 1:5){
  # Si l'Id est 0 alors vous n'avez pas rentr? un pseudo
  id.loop <- data.pseudo_id[i,"ids_joueurs"]
  pseudo.loop <- data.pseudo_id[i,"pseudos_joueurs"]
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
write.csv(team.games, file = "liste_game_team.csv") #Enregistrement

###############################
# Obtention des tableaux des joueurs des matchs avec le nom des joueurs
##############################
# je n'ai pas encore g?r? les teams
#
#
##############################

vec.id_games <- team.games$gameId
loop.tab <- data.frame()

json.tab <- NULL
stat.tab <- NULL
stats.stat.tab <- NULL

participants.tab <- NULL
player.partcipants.tab <- NULL

loop.merge <- NULL
loop.tab<- NULL

for(i in 1:length(vec.id_games)){
  id.loop <- vec.id_games[i]
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

write.csv(loop.tab, file = "stats_game_team.csv") #Enregistrement

#####################################################################
vec.players <- pseudos_joueurs
data <- team.allstats(pseudos_joueurs, serveur, key)
data.summary<-team.summary(data,"mean")
data.normalize<-team.normalize(data)
KDA <- team.kda(data = data,filtre = "win")

Column <- gvisColumnChart(resultat)
plot(Column)
