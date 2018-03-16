#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(png)
library(raster)
library(grid)
library(jpeg)
rink <- readJPEG("NHLRink.jpg")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  output$NHLPLot <- renderPlotly({
    invalidateLater(10000, session)
    gameID <- input$GameID
    gameData <- fromJSON(rawToChar(GET(url = paste("http://statsapi.web.nhl.com/api/v1/game/", gameID, "/feed/live", sep = ""))$content))
    
    PBP <- gameData$liveData$plays$allPlays
    PBP <- flatten(PBP)
    pbpfilter <- PBP[!is.na(PBP$team.triCode),]
    modifiedCoords <- pbpfilter$coordinates.x * (2*(pbpfilter$about.period %% 2) - 1)
    ggplot(data = pbpfilter, aes(x = pbpfilter$coordinates.x, y = pbpfilter$coordinates.y)) +
      annotation_raster(rink, -101, 101, -43, 43, interpolate=FALSE) + geom_point(aes(color = pbpfilter$team.triCode, shape = pbpfilter$result.event), size = 2)
    #plot_ly(data = PBP, marker = list(size = 12), x = PBP$coordinates.x, y = PBP$coordinates.y, symbol = PBP$result.event, color = as.factor(PBP$team.triCode))
    
  })
  
})
