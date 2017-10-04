joueur<-read.csv("ids_joueurs.csv")
matchlist<-read.csv("matchs_list.csv")
matchinfo<-read.csv("matchs_stats.csv")

names(matchinfo)
matchinfo<-matchinfo[c(1:65,79:87)]
matchinfo<-matchinfo[c(1:5,13:74)]
Donnees<-(data.frame(Id=matchinfo$X,
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
                     Duree=matchinfo$duration))

