library(shinydashboard)
library(shiny)
library(plotly)
library(DT)
tags$head(
  tags$style(
    type = "text/css",
    ".shiny-input-container {padding-top: 10px !important;}"
  )
)
dashboardPage(
  dashboardHeader(
    tags$li(
      class = "dropdown",
      tags$style(".main-header {max-height: 20px}"),
      tags$style(".main-header .logo {height: 20px;}"),
      tags$style(".sidebar-toggle {height: 20px; padding-top: 1px !important;}"),
      tags$style(".navbar {min-height:20px !important}")
    )
  ),
  dashboardSidebar(
    # Adjust the sidebar
    tags$style(".left-side, .main-sidebar {padding-top: 15px}"),
    tags$style(
      HTML(
        '
        .form-group, .selectize-control, .section-sidebar, .shiny-input-container, .form-group shiny-input-container {
        margin-bottom: 0px;
        margin-right: 0px;
        }
        .box-body {
        padding-bottom: 0px;
        padding-right: 0px;
        }'
)
      ),
uiOutput("audiotag"),
sidebarMenu( id = 'tab',
  menuItem("Play By Play", icon = icon("th"), tabName = "dashBoard",
           badgeLabel = "new", badgeColor = "green"),
  fluidRow(
    column(
      offset = 0,
      style = 'padding-left:15px; padding-right:0px',
      selectInput(
        inputId = "Season",
        label = "Pick Season",
        choices = c("20172018", "20162017", "20152016")
      ),
      width = 7
    ),
    
    column(
      offset = 0,
      style = 'padding-right:10px; padding-left:0px',
      selectizeInput(
        inputId = "Team",
        label = "Pick Team",
        choices = c()
      ),
      width = 5
    )
  ),
  selectizeInput(
    inputId = "GameNumber",
    label = "Select a game",
    choices = c(),
    options = list(maxOptions = 2000)
  ),
  sliderInput(
    inputId = "Refresh",
    label = "Refresh Rate",
    min = 3,
    max = 60,
    value = 5,
    step = 5
  ),
  checkboxGroupInput(
    "plotVars",
    "On Ice Variables:",
    c(
      "Goals" = "Goal",
      "Shots" = "Shot",
      "Hits" = "Hit",
      "Penalties" = "Penalty",
      "Faceoffs" = "Faceoff",
      "Blocks" = "Blocked Shot",
      "Misses" = "Missed Shot",
      "Takeaways" = "Takeaway",
      "Giveaways" = "Giveaway"
    ),
    selected = c("Goal", "Shot", "Hit"),
    inline = TRUE
  ),
  menuItem("NHL.com", icon = icon("th"), tabName = "NHLcom",
           badgeLabel = "new", badgeColor = "green"),
  width = 2
)
      ),
dashboardBody(
  tabItems(
    tabItem( tabName = "dashBoard", 
      
    
  fluidRow(
  column(
    width = 3,
    box(htmlOutput("picture")),
    box(htmlOutput("picture2")),
    box(div(tableOutput("TeamStats"), style = "font-size: 82%"), width = 12),
    tabsetPanel(tabPanel(title = "Home", box(div(dataTableOutput("rosters"), style = "font-size: 80%"),  width = 12)), tabPanel(title = "Away", box(div(dataTableOutput("awayRoster"), style = "font-size: 80%"),  width = 12)), id = "gameRosters", type = "tabs")
  ),
  column(
    width = 9,
    box(plotlyOutput("NHLPLot", height = 300), width = 12),
    box(htmlOutput("Player"), width = 12),
    box(dataTableOutput("PlaySummary"), width = 12)
  )
  )
  ),
  tabItem(tabName = "NHLcom", htmlOutput("NHLSite"))
))
    )

