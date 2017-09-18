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

#Récupérer la liste des matchs classés d'un joueur
result.matchslist <- lol.matchslist.ranked.easy(account.id, serveur, key)
matchs.list <- result.matchslist[[1]]



###################################################################
#Test Adresse des images et version

version<-lol.staticdata.version(server,key)[1]
square<-lol.staticdata.image("Urgot","square",0,version)
loading<-lol.staticdata.image("Urgot","loading",1,version)
