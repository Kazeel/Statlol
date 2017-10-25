#############################################################################
#Class LOL Player
#############################################################################
# You need this packages :

# library(jsonlite)
# library(curl)
# library(httr)
############################################################################
# Functions using Riot APi
#############################################################################

#############################################################################
# Class Player
############################################################################

setClass(
  Class = "Player",
  representation = representation(
    profileIconId = "character",
    name = "character",
    summonerLevel= "numeric",
    accountId = "character",
    id =  "character",
    revisionDate = "character",
    server = "character"
  )
)
###############################################################################
# Basic Methods
##############################################################################
setMethod("print","Player",
          function(x,...){
            cat("*** Class Player, method Print ***\n")
            cat("* profileIconId = ");print(x@profileIconId)
            cat("* name = ");print(x@name)
            cat("* summonerLevel= ");print(x@summonerLevel)
            cat("* accountId= ");print(x@accountId)
            cat("* id= ");print(x@id)
            cat("* revisionDate= ");print(x@revisionDate)
            cat("* revisionDate= ");print(x@server)
            cat("***** Fin Print(Player) *****\n")
          }
)

setMethod("show","Player",
          function(object){
            cat("*** Class player, method Print ***\n")
            cat("* profileIconId = ");print(object@profileIconId)
            cat("* name = ");print(object@name)
            cat("* summonerLevel= ");print(object@summonerLevel)
            cat("* accountId= ");print(object@accountId)
            cat("* id= ");print(object@id)
            cat("* revisionDate= ");print(object@revisionDate)
            cat("* revisionDate= ");print(object@server)
            cat("***** Fin Print(trajectoires) *****\n")
          }
)
########################################################################
# Constructors
########################################################################

setGeneric(name = "player", def = function (name, accountId, id, apikey, server, ...) {standardGeneric("player")})

setMethod(
  f = "player", 
  signature = signature(name = "character", accountId = "missing", id = "missing", apikey="character", server = "character"),
  definition = function (name,apikey, server) {
    # return a Player Class-Object, or a string with the error message
    
    #Transformation of data
    pseudo<- tolower(name)
    pseudo<- gsub(" ", "", pseudo, fixed = TRUE)
    
    #Retriving from api
    url<-paste("https://",server,".api.riotgames.com/lol/summoner/v3/summoners/by-name/",pseudo,"?api_key=",apikey,sep="")
    liste<- content(GET(url), as="parsed")
    
    #Handle error type
    try(if ( "status" %in% names(liste) ){
      msg <- paste(liste$status$message,", error :",liste$status$status_code)
      warning(msg)
      return(msg)
    } )
    
    #Generate Player if everything is fine
    obj <- new(Class="Player",
               profileIconId = as.character(liste$profileIconId),
               name = liste$name,
               summonerLevel= liste$summonerLevel,
               accountId = as.character(liste$accountId),
               id =  as.character(liste$id),
               revisionDate = as.character(liste$revisionDate),
               server = server)
    return(obj)
})

setMethod(
  f = "player", 
  signature = signature(name = "missing", accountId = "character", id = "missing", apikey="character", server = "character"),
  definition = function (accountId, apikey, server) {
    # return a Player Class-Object, or a string with the error message
    
    #Retriving from api
    url<-paste("https://",server,".api.riotgames.com/lol/summoner/v3/summoners/by-account/",accountId,"?api_key=",apikey,sep="")
    liste<- content(GET(url), as="parsed")
    
    #Handle error type
    try(if ( "status" %in% names(liste) ){
      msg <- paste(liste$status$message,", error :",liste$status$status_code)
      warning(msg)
      return(msg)
    } )
    
    #Generate Player if everything is fine
    obj <- new(Class="Player",
               profileIconId = as.character(liste$profileIconId),
               name = liste$name,
               summonerLevel= liste$summonerLevel,
               accountId = as.character(liste$accountId),
               id =  as.character(liste$id),
               revisionDate = as.character(liste$revisionDate),
               server = server)
    return(obj)
  })

setMethod(
  f = "player", 
  signature = signature(name = "missing", accountId = "missing", id = "character", apikey="character", server = "character"),
  definition = function (id, apikey, server) {
    # TODO
  })
