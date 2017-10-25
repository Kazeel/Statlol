#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

source("./fonctions_lol.R", local = TRUE)
source("./fonctions_team.R", local = TRUE)
require(jsonlite)
require(curl)
require(httr)
require(TriMatch)
require(googleVis)
library(shiny)
library(DT)

# Define UI for application that draws a histogram
ui <- navbarPage(
   
   # Application title
   "My LOL Team",
   
   tabPanel("Overview",
     sidebarLayout(
     sidebarPanel(
       textInput("apikey", "Riot Api key",""),
       textInput("topname", "Top name","RKSReidoz"),
       textInput("junname", "Jungler name","Kazeel"),
       textInput("midname", "Mid name","Mashu"),
       textInput("adcname", "Adc name","Redwhale"),
       textInput("supname", "Sup name","TheDeathcookie"),
       actionButton("analyze", "Analyze")
       
       
     ),
     #################################################################################
     # Show a plot of the generated distribution
     mainPanel(tabsetPanel(
       tabPanel("Winrate", 
                fluidRow(
                  column(4,htmlOutput("winrate_summary")),
                  column(4,
                         htmlOutput("winrate_Bside"),
                         htmlOutput("winrate_Rside")),
                  column(4)
                  )),
       tabPanel("Farm",
                fluidRow(
                  column(8,htmlOutput("farm_summary")),
                  column(8,htmlOutput("farm_win"))
                  )
                ),
       tabPanel("KDA",
                fluidRow(
                  column(8,htmlOutput("kda_summary")),
                  column(8,htmlOutput("kda_win"))
                  )
                ),
       tabPanel("Data", DT::dataTableOutput("fulltable"))
     )
     
     )
   ))
   # Sidebar with a slider input for number of bins 
   
)

################################################################################################################
# SERVER
#################################################################################################################

server <- function(input, output) {
  
  observeEvent(input$analyze, { #reaction to analyze button
    
    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    #First Progress
    progress$set(message = "Fetch Data", value = 1/3)
    
    #Generate data
    data<-team.allstats(c(input$topname,input$junname,input$midname,input$adcname,input$supname),"euw1",input$apikey)
    
    # Last progress
    progress$inc(1/10, detail = "Render")
    
    #######################################################
    #Output Winrate_summary for Overview
    output$winrate_summary <- renderGvis({
      gvisPieChart(data = team.winrate(data), 
                   labelvar = "Win", 
                   numvar = "Winrate", 
                   options = list(
                     title ="Winrate of the team",
                     legend = {position = 'none'},
                     width=300, height=300,
                     colors="['#156711','#540002']"))
    })#End Output
    #######################################################
    progress$inc(1/10, detail = "calculate Winrate")
    winrate_side <- team.winrate(data,"side")
    #######################################################
    #Output Winrate_Bside for Overview
    output$winrate_Bside <- renderGvis({
      gvisPieChart(data = winrate_side[,c("Win","Blue")], 
                   labelvar = "Win", 
                   numvar = "Blue", 
                   options = list(
                     title ="Winrate when team play on Blue Side",
                     legend = {position = 'none'},
                     width=300, height=300,
                     colors="['#156711','#540002']"))
    })#End Output
    #######################################################
    #Output Winrate_Rside for Overview
    output$winrate_Rside <- renderGvis({
      gvisPieChart(data = winrate_side[,c("Win","Red")], 
                   labelvar = "Win", 
                   numvar = "Red", 
                   options = list(
                     title ="Winrate when team play on Red Side",
                     legend = {position = 'none'},
                     width=300, height=300,
                     colors="['#156711','#540002']"))
    })#End Output
    #######################################################
    progress$inc(1/10, detail = "calculate farming")
    #######################################################
    #Output Farm_win for Overview
    output$farm_summary <- renderGvis({
      gvisColumnChart(data = team.farm(data),
                   options = list(
                     title ="Minions kill (including jungle) per Minute",
                     legend = {position = 'none'}))
    })#End Output
    #######################################################
    #Output Farm_summary for Overview
    output$farm_win <- renderGvis({
      gvisColumnChart(data = team.farm(data,"win"), 
                      options = list(
                        title ="Minions kill per Minute depending of Victory",
                        legend = {position = 'none'},
                        colors="['#156711','#540002']"))
    })#End Output
    #######################################################
    progress$inc(1/10, detail = "calculate KDA")
    #######################################################
    #Output Kda summary for Overview
    output$kda_summary <- renderGvis({
      gvisColumnChart(data = team.kda(data, onenumbers = "yes"),
                      options = list(
                        title ="KDA",
                        legend = {position = 'none'}))
    })#End Output
    #######################################################
    #Output Kda win for Overview
    output$kda_win <- renderGvis({
      gvisColumnChart(data = team.kda(data=data,filtre="win",onenumbers = "yes"), 
                      options = list(
                        title ="KDA depending of Victory",
                        legend = {position = 'none'},
                        colors="['#156711','#540002']"))
    })#End Output
    #######################################################
    #Output table
    output$fulltable = DT::renderDataTable({
      data
    }) # End table
    
    #Output table
    output$meantable = DT::renderDataTable({
      team.summary(data,"mean")
    }) # End table
    #######################################################
    #Output Plot
    output$plotKDA <- renderGvis({
      gvisColumnChart(summary[,c("Group.1","Kill","Death","Assist")],options=list(width=670, height=600))
    })#End Plot
    
  })## End ObserveEvent
   
  
} # End Server

############################################################################################################
# RUN
#############################################################################################################
# Run the application 
shinyApp(ui = ui, server = server)

