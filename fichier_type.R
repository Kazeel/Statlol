#############################
#Programme type
###########################
#
#
#
#
###########################
# Données utilisateurs
##########################
#
#
#
############################

pseudo <- "Kazeel"
serveur <- "euw1"
saison <- 2016
key    <- "RGAPI-6821158c-1c77-4553-be1c-26e7c54aff6f"

##########################
# Fonctions
#########################
#
#
#############################
dir <- "C:/Documents/GitHub/Statlol"
chemin<- file.path(dir,"fonctions_lol.R")
source(file = chemin)
library(jsonlite)
library(curl)
library(httr)
library(TriMatch)

#####################################
#Algrotihme
####################################
#
#
#####################################

#récupérer l'ID
result.id<-  lol.idjoueur(pseudo, serveur, key)
account.id<-result.id[[2]]

#récupérer les stats
result.stats<- lol.statsjoueur(id, serveur, saison, key)
stats.list<- result.stats[[3]]
stats.table<-matrix(data = unlist(stats.list),nrow = nrow(stats.list),ncol=length(unlist(stats.list))/nrow(stats.list),byrow = FALSE,dimnames = list(c(),names(cbind(stats.list[1],stats.list$stats))))
data<-stats.table[-which(stats.table[,1]==0),-c(8, 10:14, 17,21,23:35)]

names(data)<-c("champions","parties jouées","parties perdues", "parties gagnées", "kill total", "dommages infligés", "dommages subis",
               "Kill max","Total Sbires tués", "Total Double kill", "Total Triple Kill", "Total Quadra kill", "Total Pentakills",
               "total Unreal Kill", "Total de mort", "Total d'Or", "Total Tours détruitent", "Total dommages physiques",
               "Total Dommage magique", "Total Assiste", "Max Kill", "Max mort")
data[,-c(10,11,12,13,14,21,22)]

#Récupérer les champions

champions<-lol.basechampions(serveur,key)

# names(champions[[2]])<-champions[[1]]

# champions[[2]][sort(champions[[1]])]

table.champions<- cbind(champions[[1]],champions[[2]])

table.champions[,1][match(data[,1],table.champions[,2])]

###################################################################
#Test Adresse des images et version

version<-lol.staticdata.version(server,key)[1]
square<-lol.staticdata.image("Urgot","square",0,version)
loading<-lol.staticdata.image("Urgot","loading",1,version)
