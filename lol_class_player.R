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
    summonername = "character",
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
            
          }
)