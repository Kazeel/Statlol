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
    profileIconId = "numeric",
    name = "character",
    summonerLevel= "numeric",
    accountId = "character",
    id =  "character",
    revisionDate = "numeric"
  )
)
###############################################################################
# Basic Methods
##############################################################################
setMethod("print","Player",
          function(x,...){
            cat("*** Class player, method Print ***\n")
            cat("* profileIconId = ");print(x@profileIconId)
            cat("* name = ");print(x@name)
            cat("* summonerLevel= ");print(x@summonerLevel)
            cat("* accountId= ");print(x@accountId)
            cat("* id= ");print(x@id)
            cat("* revisionDate= ");print(x@revisionDate)
            cat("***** Fin Print(trajectoires) *****\n")
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
            cat("***** Fin Print(trajectoires) *****\n")
          }
)
########################################################################
# Constructors
########################################################################

setGeneric(name = "player", def = function (name, accountId, id, ...) {standardGeneric("player")})

setMethod(
  f = "player", 
  signature = signature(name = "character", accountId = "missing", id = "missing"),
  definition = function (name) {
    # TODO
})

setMethod(
  f = "player", 
  signature = signature(name = "missing", accountId = "character", id = "missing"),
  definition = function (name) {
    # TODO
  })

setMethod(
  f = "player", 
  signature = signature(name = "missing", accountId = "missing", id = "character"),
  definition = function (name) {
    # TODO
  })