joueur<-read.csv("ids_joueurs.csv")
matchlist<-read.csv("matchs_list.csv")
matchinfo<-read.csv("matchs_stats.csv")

names(matchinfo)
matchinfo<-matchinfo[c(1:65,79:87)]
matchinfo<-matchinfo[c(1:5,13:74)]
Data<-data.frame(Id=matchinfo$X,
                     Id_Part= matchinfo$participantId,
                     Id_Team=matchinfo$teamId,
                     Win=matchinfo$win,
                     Kill=matchinfo$kills,
                     Death=matchinfo$deaths,
                     Assist=matchinfo$assists,
                     Score_Vision=matchinfo$visionScore,
                     Damage_D=matchinfo$totalDamageDealt,
                     Heal=matchinfo$totalHeal,
                     Damage_T=matchinfo$totalDamageTaken,
                     Gold_E=matchinfo$goldEarned,
                     Gold_S=matchinfo$goldSpent,
                     Turret=matchinfo$turretKills,
                     Inhib=matchinfo$inhibitorKills,
                     Minions=matchinfo$totalMinionsKilled,
                     Minionsbis=matchinfo$neutralMinionsKilled,
                     F_Blood=matchinfo$firstBloodKill,
                     F_Tower=matchinfo$firstTowerKill,
                     Id_Ac=matchinfo$accountId,
                     Id_Sum=matchinfo$summonerId,
                     Duree=matchinfo$duration,
                     Id_game=substr(matchinfo$X,1,10))

Data$Id_game<-levels(Data$Id_game)#A corriger !


Game<-function(Id,etat,duree){
  #Création et constructeur de la classe Game qui est définie par un id (unique), un état et une durée
  Constructeur<-data.frame(etat=etat,duree=duree)
  Constructeur$Id<-Id
  class(Constructeur)<-"Game"
  Constructeur
}
G1<-Game(Data$Id_game[1],Data$Win[1],Data$Duree[1])
print.Game<-function(game){
  cat("La Game",game$Id,"a durée",game$duree,"s \n")#A Corriger.
}
G1

#Transformer Une variable en Factor pour la variable Win
Test<-factor(Data$Win)
attributes(Test)$levels<-c("Perdu","Gagné")
Test
