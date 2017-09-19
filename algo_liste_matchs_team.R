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

write.csv(data.pseudo_id, file = "ids_joueurs.csv") #Enregristrement