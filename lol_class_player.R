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
#
############################################################################

setClass(
  Class = "player",
  representation = representation(
    profileIconId = "numeric",
    name = "character",
    summonerLevel= "numeric",
    accountId = "character",
    id =  "character",
    revisionDate = "numeric"
  )
)

setMethod("print","player",
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

setMethod("show","player",
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

setGeneric(
  name = 
)