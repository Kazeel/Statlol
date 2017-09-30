#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

source("../fonctions_lol.R", local = TRUE)
source("../fonctions_team.R", local = TRUE)
require(jsonlite)
require(curl)
require(httr)
require(TriMatch)
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("My LOL Team"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        textInput("apikey", "Apikey","Api Key Riot"),
        textInput("topname", "Topname","Pseudo du Toplaner"),
        textInput("junname", "Junname","Pseudo du Jungler"),
        textInput("midname", "Midname","Pseudo du Midlaner"),
        textInput("adcname", "Adcname","Pseudo de l'Adc"),
        textInput("supname", "Supname","Pseudo du Support")
        
          
      ),
      #################################################################################
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)
#################################################################################################################
# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

