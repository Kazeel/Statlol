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
   
   tabPanel("Information",
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
       tabPanel("Plot", htmlOutput("plotKDA")),
       tabPanel("Summary", DT::dataTableOutput("meantable")),
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
    progress$inc(1/4, detail = "Render")
    
    summary <- team.summary(data,"mean")
    
    
    #Output table
    output$fulltable = DT::renderDataTable({
      data
    }) # End table
    
    #Output table
    output$meantable = DT::renderDataTable({
      summary
    }) # End table
    
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

