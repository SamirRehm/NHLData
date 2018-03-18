#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
library(shiny)
library(png)
library(raster)
library(grid)
library(jpeg)
library(plotly)
library(jsonlite)
library(httr)
library(htmlwidgets)
library(plyr)
library(ggplot2)
library(gridExtra)
library(gtable)
library(reshape2)
library(DT)
rink <- readJPEG("NHLRink.jpg")
setwd("C:/Users/Samir Rehmtulla/Documents/NHLData/NHLData")
# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  print(session)
  
  plays = reactive({
    invalidateLater(10000, session)
    gameID <- input$GameID
    gameData <- jsonlite::fromJSON(rawToChar(httr::GET(url = paste("http://statsapi.web.nhl.com/api/v1/game/", gameID, "/feed/live", sep = ""))$content))
    PBP <- gameData$liveData$plays$allPlays
    PBP <- flatten(PBP)
  })
  
  
  output$NHLPLot <- renderPlotly({
    PBP <- plays()
    pbpfilter <- PBP[!is.na(PBP$team.triCode),]
    modifiedCoords <- pbpfilter$coordinates.x * (2*(pbpfilter$about.period %% 2) - 1)
    p <- ggplot( data = pbpfilter, aes(x = modifiedCoords, y = pbpfilter$coordinates.y)) + annotation_raster(rink, -100, 100, -42.5, 42.5, interpolate=FALSE) +
    geom_point(aes(color = pbpfilter$team.triCode, shape = pbpfilter$result.event), size = 2) +
      scale_shape_manual(values = c(7, 13, 3, 11, 20, 21, 22, 23, 24, 25)) +  
      theme(legend.title=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(),axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank()) +
      theme(legend.position = "bottom", legend.text = element_text(size = 5)) + theme(legend.key = element_rect(colour = "blue"))
    p <- ggplotly(p)
    p
  })
  
  output$PlaySummary <- renderDataTable({
    PBP <- plays()
    w <- data.frame(PBP$about.periodTimeRemaining, PBP$team.triCode, PBP$result.event, PBP$result.description)
    w<-w[dim(w)[1]:1,]
    datatable(w, options = list(dom = 'tp'))
  })
  
  output$TeamStats <- renderTable({
    PBP <- plays()
    PBP <- PBP[!is.na(PBP$team.triCode),]
    PBP$result.event <- factor(PBP$result.event, levels = c("Goal", "Shot", "Penalty", "Hit", "Faceoff", "Blocked Shot", "Missed Shot", "Takeaway", "Giveaway"))
    teams <- unique(PBP$team.triCode)
    PBPTeam1 <- PBP[PBP$team.triCode == teams[[1]],]
    PBPTeam2 <- PBP[PBP$team.triCode == teams[[2]],]
    Team1Stats <- table(PBPTeam1$result.event)
    Team2Stats <- table(PBPTeam2$result.event)
    x <- data.frame(Team1Stats, Team2Stats)
    y <- data.frame(x$Var1, x$Freq, x$Freq.1)
    y <- plyr::rename(y, c("x.Var1" = "Stat", "x.Freq" = teams[[1]], "x.Freq.1" = teams[[2]]))
  }, alig = 'r')
})
