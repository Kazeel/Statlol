joueur<-read.csv("ids_joueurs.csv")
matchlist<-read.csv("matchs_list.csv")
matchinfo<-read.csv("matchs_stats.csv")

#Parametre
P1<-"Kazeel"
P2<-"Red Whale"
P3<-"Mashu"
P4<-"SweepzKillz"
P5<-"RKS Reidoz"
listP<-c(P1,P2,P3,P4,P5)

###AJOUTER FONCTION RESTRICTION DONNEES ENFONCTION DESPLAYERS 
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
                     Damage_D=matchinfo$totalDamageDealtToChampions,
                     CC=matchinfo$totalTimeCrowdControlDealt,
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
                     Name=matchinfo$summonerName,
                     Id_Sum=matchinfo$summonerId,
                     Duree=matchinfo$duration,
                     Id_game=substr(matchinfo$X,1,10))

Data$Id_game<-levels(Data$Id_game)#A corriger !

#Calc Preparation formule
J1<-Data[Data$Name=="Kazeel",]
TxWin<-sum(J1$Win)/length(J1$Win)*100 #Taux de victoire en pourcentage.
KDA<-(sum(Data$Kill[Data$Name==P1])+sum(Data$Assist[Data$Name==P1]))/sum(Data$Death[Data$Name==P1])
Damage<-(sum(Data$Damage_D[Data$Name==P1]))/length(Data[Data$Name==P1,])
DamageT<-(sum(Data$Damage_T[Data$Name==P1]))/length(Data[Data$Name==P1,])
Heal<-(sum(Data$Heal[Data$Name==P1]))/length(Data[Data$Name==P1,])
Gold<-(sum(Data$Gold_E[Data$Name==P1]))/length(Data[Data$Name==P1,])
CC<-(sum(Data$CC[Data$Name==P1]))/length(Data[Data$Name==P1,])
Vision<-(sum(Data$Score_Vision[Data$Name==P1]))/length(Data[Data$Name==P1,])
Farm<-(sum(Data$Minions[Data$Name==P1]))/length(Data[Data$Name==P1,])+(sum(Data$Minionsbis[Data$Name==P1]))/length(Data[Data$Name==P1,])



Player<-function(Name,Data){
  C<-list(Name=Name)
  C$TxWin<-sum(Data[Data$Name==C$Name,]$Win)/length(Data[Data$Name==C$Name,]$Win)*100 #Taux de victoire en pourcentage.
  C$KDA<-(sum(Data$Kill[Data$Name==C$Name])+sum(Data$Assist[Data$Name==C$Name]))/sum(Data$Death[Data$Name==C$Name])
  C$Damage<-(sum(Data$Damage_D[Data$Name==C$Name]))/length(Data[Data$Name==C$Name,])
  C$DamageT<-(sum(Data$Damage_T[Data$Name==C$Name]))/length(Data[Data$Name==C$Name,])
  C$Heal<-(sum(Data$Heal[Data$Name==C$Name]))/length(Data[Data$Name==C$Name,])
  C$Gold<-(sum(Data$Gold_E[Data$Name==C$Name]))/length(Data[Data$Name==C$Name,])
  C$CC<-(sum(Data$CC[Data$Name==C$Name]))/length(Data[Data$Name==C$Name,])
  C$Vision<-(sum(Data$Score_Vision[Data$Name==C$Name]))/length(Data[Data$Name==C$Name,])
  C$Farm<-(sum(Data$Minions[Data$Name==C$Name]))/length(Data[Data$Name==C$Name,])+(sum(Data$Minionsbis[Data$Name==C$Name]))/length(Data[Data$Name==C$Name,])
  class(C)<-"Player"
  C
}
Play1<-Player(P1,Data);Play2<-Player(P2,Data);Play3<-Player(P3,Data);Play4<-Player(P4,Data);Play5<-Player(P5,Data)

Equip<-function(equip,data){
  #equip=vecteur conteant la lsite des 3 ou5 joueur de l'équipe
  if(typeof(equip)=="character"&(length(equip)==5|length(equip)==3)){
    #Restriction donnees au joueur
    for (i in 1:length(equip)){
      x<-paste("Play",i,sep="")
      assign(x,Player(equip[i],data))
    }
  C<-list(ListPlay=equip)
  C$TxWin<-Play1$TxWin
  C$KDA<-mean(Play1$KDA,Play2$KDA,Play3$KDA,Play4$KDA,Play5$KDA)
  C$Damage<-mean(Play1$Damage,Play2$Damage,Play3$Damage,Play4$Damage,Play5$Damage)
  C$DamageT<-mean(Play1$DamageT,Play2$DamageT,Play3$DamageT,Play4$DamageT,Play5$DamageT)
  C$Heal<-mean(Play1$Heal,Play2$Heal,Play3$Heal,Play4$Heal,Play5$Heal)
  C$Gold<-mean(Play1$Gold,Play2$Gold,Play3$Gold,Play4$Gold,Play5$Gold)
  C$CC<-mean(Play1$CC,Play2$CC,Play3$CC,Play4$CC,Play5$CC)
  C$Vision<-mean(Play1$Vision,Play2$Vision,Play3$Vision,Play4$Vision,Play5$Vision)
  C$Farm<-mean(Play1$Farm,Play2$Farm,Play3$Farm,Play4$Farm,Play5$Farm)
  class(C)<-c("Equip","Player")
  C}
  else{print("Erreur: Paramètre equipe non-adapté!")}
}

Equip1<-Equip(listP,data)
plot.Equip<-function(equip){
  
}






Game<-function(Id,Etat,Duree){
  #Création et constructeur de la classe Game qui est définie par un id (unique), un état et une durée
  Constructeur<-list(Etat=Etat,Duree=Duree)
  Constructeur$Id<-Id
  class(Constructeur)<-"Game"
  Constructeur
}

#Exemple 1
G1<-Game(Data$Id_game[1],Data$Win[1],Data$Duree[1])
print.Game<-function(game){
  cat("La Game",game$Id,"a durée",game$Duree,"s \n")#A Corriger.
}
G1

#Transformer Une variable en Factor pour la variable Win
Test<-factor(Data$Win)
attributes(Test)$levels<-c("Perdu","Gagné")
Test

for(i in  1L:5L){
  print(cat("P",i,sep=""))
  assign(as.character(i),2)
  print(as.character(i))
  }
