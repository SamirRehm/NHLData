#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(  fluid = TRUE,
    sidebarPanel(
       numericInput(inputId = "GameID", label = "Enter Game ID", value = 10), width = 3,
       tableOutput("TeamStats")
    ),
    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("NHLPLot", height = 200), div(dataTableOutput("PlaySummary"), style = "font-size: 75%; height: 75%"), fluid = TRUE
    )
  )
))

