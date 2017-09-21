#############################
# Algorithme d'extraction des matchs d'une équipe
###########################
#
#
#
#
###########################
# Données utilisateurs & sourcing
##########################
#
#
#
############################

serveur <- "euw1"
key    <- "RGAPI-ea3a2952-91ac-48be-96c0-0bcd83f505a9"

############################
# Données joueurs
############################
# Penser à retirer les espaces
#
#
############################

pseudo_top <- "RKSReidoz"
pseudo_jun <- "Kazeel"
pseudo_mid <- "Mashu"
pseudo_adc <- "RedWhale"
pseudo_sup <- ""

###########################
# Génération de la liste des joueurs et de leurs account.id
###########################
#
#
#
###############################

pseudos_joueurs <- c("TOP"= pseudo_top, "JUN"= pseudo_jun, "MID"=pseudo_mid, "ADC"=pseudo_adc,"SUP"=pseudo_sup)
ids_joueurs <- c()
for(i in 1:5){
  # Si l'Id est 0 alors vous n'avez pas rentré un pseudo
  if(pseudos_joueurs[i] != "" ){
    ids_joueurs[i]<-lol.idjoueur(pseudo = pseudos_joueurs[i], serveur = serveur, key = key)[["accountId"]]
  } else{
    ids_joueurs[i] <- 0
  }
}

data.pseudo_id <- data.frame(pseudos_joueurs, ids_joueurs)

write.csv(data.pseudo_id, file = "ids_joueurs.csv") #Enregistrement

###########################
# Récupération de la liste des matchs de chaque joueur
###########################
#
#
###############################

listes_matchs <- data.frame()
for(i in 1:5){
  # Si l'Id est 0 alors vous n'avez pas rentré un pseudo
  id.loop <- data.pseudo_id[i,"ids_joueurs"]
  pseudo.loop <- data.pseudo_id[i,"pseudos_joueurs"]
  if(id.loop != 0 ){
    listes_matchs<- rbind(listes_matchs,lol.matchslist.ranked.easy(id.loop, serveur, key)[[1]])
  } 
}

flex.listes_matchs <- subset(listes_matchs, queue == 440)
team.games <- unique(flex.listes_matchs[,c(1,2,5,6)])
write.csv(team.games, file = "liste_game_team.csv") #Enregistrement

###############################
# Obtention des tableaux des joueurs des matchs avec le nom des joueurs
##############################
# je n'ai pas encore géré les teams
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
      
      row.names(loop.merge) <- paste(id.loop, c(0:9), sep = "")
  
      loop.tab<- rbind(loop.tab,loop.merge)
    }
  } 
}

write.csv(loop.tab, file = "stats_game_team.csv") #Enregistrement



