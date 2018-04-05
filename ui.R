#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

#library(shiny)
#library(plotly)

# Define UI for application that draws a histogram
#shinyUI(fluidPage(
  
  # Sidebar with a slider input for number of bins 
  #sidebarLayout(  fluid = TRUE,
  #  sidebarPanel(
  #     numericInput(inputId = "GameID", label = "Enter Game ID", value = 10), width = 3,
  #     div(tableOutput("TeamStats"), style = "font-size: 65%")
  #  ),
    # Show a plot of the generated distribution
  #  mainPanel(
  #    plotlyOutput("NHLPLot"), div(dataTableOutput("PlaySummary"), style = "font-size: 65%; width: 40%; height: 75%"), fluid = TRUE
  #  )
#  )
#))

library(shinydashboard)
library(shiny)
library(plotly)
library(DT)

dashboardPage(
  dashboardHeader(
    # Set height of dashboardHeader
    tags$li(class = "dropdown",
            tags$style(".main-header {max-height: 20px}"),
            tags$style(".main-header .logo {height: 20px;}"),
            tags$style(".sidebar-toggle {height: 20px; padding-top: 1px !important;}"),
            tags$style(".navbar {min-height:20px !important}")
    ) 
  ),
  dashboardSidebar(
    # Adjust the sidebar
    tags$style(".left-side, .main-sidebar {padding-top: 20px}"),
    uiOutput("audiotag"),
    #tags$audio(src = "a.mp3", type = "audio/wav", autoplay = NA),
    #output$audiotag<-renderUI(get_audio_tag("tempwav.wav")), #starting wave file 
    sidebarMenu(
      selectInput(inputId = "Season", label = "Select a season", choices = c("20172018", "20162017", "20152016")),
      selectizeInput(inputId = "Team", label = "Select a team", choices = c()),
      
      selectizeInput(inputId = "GameNumber", label = "Select a game", choices = c(), options = list(maxOptions = 2000)),
      sliderInput(inputId = "Refresh", label = "Refresh Rate", min = 3, max = 60, value = 5, step = 5),
      checkboxGroupInput("plotVars", "On Ice Variables:",
                         c("Goals" = "Goal",
                           "Shots" = "Shot",
                           "Hits" = "Hit",
                           "Penalties" = "Penalty",
                           "Faceoffs" = "Faceoff",
                           "Blocks" = "Blocked Shot",
                           "Misses" = "Missed Shot",
                           "Takeaways" = "Takeaway",
                           "Giveaways" = "Giveaway")), width = 3
    )
  ),
  dashboardBody(
    fluidRow(
      column( width = 3,
              box(htmlOutput("picture")),
              box(htmlOutput("picture2")),
             box(div(tableOutput("TeamStats"), style = "font-size: 82%"), width = 12)
             ),
      column(width = 9, box(plotlyOutput("NHLPLot", height = 300), width = 12),
             box(htmlOutput("Player"), width = 12),
             box(dataTableOutput("PlaySummary"), width = 12))
      
  )
)
)

