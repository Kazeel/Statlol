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
team.games <- flex.listes_matchs[duplicated(flex.listes_matchs$gameId, fromLast = TRUE),c(1,2,5,6)]
write.csv(team.games, file = "liste_game_team.csv") #Enregistrement
