####################################
# THIS ALGO IS USE TO WORK WITH GOOGLE SHEET
####################################
#
#
#
#
####################################
# SOURCING
####################################

require("googlesheets")
suppressPackageStartupMessages(library("dplyr"))

###################################
# TEST
#####################################

gs_gap() %>% gs_copy(to = "Gapminder") # Data test

gap <- gs_title("Gapminder")
