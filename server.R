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
library(tuneR)
numGoals <- 0
gameID <- 10
lastGoalTime <- Sys.time() -1000000
u <- as.numeric(difftime(lastGoalTime, Sys.time(), units = 'secs'))
rink <- readJPEG("NHLRink.jpg")
shinyServer(function(input, output, session) {
  get_audio_tag<-function(filename){tags$audio(src = filename,
                                               type ="audio/wav", autoplay = NA)}
  
  wave_name<-function(){
    paste0( paste("a",
                   ".mp3", sep = ""))
  }

     #starting wave file 
  
  plays = reactive({
    invalidateLater(10000, session)
    gameData <- jsonlite::fromJSON(rawToChar(httr::GET(url = paste("http://statsapi.web.nhl.com/api/v1/game/", input$GameID, "/feed/live", sep = ""))$content))
    
    if(input$GameID != gameID) {
      src = paste("//www-league.nhlstatic.com/nhl.com/builds/site-core/33f4bcacaa52eed691a6f0671c4cde69850f3c31_1521478002/images/logos/team/current/team-", gameData$gameData$teams$home$id,"-dark.svg", sep="")
      output$picture<-renderText({c('<img src="',src,'">')})
      src2 = paste("//www-league.nhlstatic.com/nhl.com/builds/site-core/33f4bcacaa52eed691a6f0671c4cde69850f3c31_1521478002/images/logos/team/current/team-", gameData$gameData$teams$away$id ,"-dark.svg", sep="")
      output$picture2<-renderText({c('<img src="',src2,'">')})
    }
    if(as.numeric(difftime(Sys.time(), lastGoalTime, units = 'secs')) > 40) {
    if(numGoals != gameData$liveData$boxscore$teams$home$teamStats$teamSkaterStats$goals + gameData$liveData$boxscore$teams$away$teamStats$teamSkaterStats$goals) 
    {
      output$audiotag<-renderUI(get_audio_tag("a.mp3"))
      lastGoalTime <<- Sys.time()
    } else {
      output$audiotag<-renderUI(get_audio_tag("none.mp3"))
    }
    }
    
    numGoals <<- gameData$liveData$boxscore$teams$home$teamStats$teamSkaterStats$goals + gameData$liveData$boxscore$teams$away$teamStats$teamSkaterStats$goals
    gameID <<- input$GameID
    PBP <- gameData$liveData
  })
  
  
  output$NHLPLot <- renderPlotly({
    gameData1 <- plays()
    PBP <- flatten(gameData1$plays$allPlays)
    variables <- input$plotVars
    pbpfilter <- PBP[!is.na(PBP$team.triCode),]
    pbpfilter <- pbpfilter[pbpfilter$result.event %in% variables,]
    modifiedCoords <- pbpfilter$coordinates.x * (2*(pbpfilter$about.period %% 2) - 1)
    Event <- pbpfilter$result.description
    p <- ggplot(data = pbpfilter, aes(x = modifiedCoords, y = pbpfilter$coordinates.y, text = Event)) + annotation_raster(rink, -100, 100, -42.5, 42.5, interpolate=FALSE) +
    geom_point(aes(color = pbpfilter$team.triCode, shape = pbpfilter$result.event), size = 2) +
      scale_shape_manual(values = c(7, 13, 3, 11, 20, 21, 22, 23, 24, 25)) +  
      theme(legend.title=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(),axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank()) +
      theme(legend.position = "bottom", legend.text = element_text(size = 7)) + theme(legend.key = element_rect(colour = "blue")) + theme(plot.margin = unit(c(0,0,0,0), "cm")) + ggtitle(paste(gameData1$boxscore$teams$away$team$name, "at",  gameData1$boxscore$teams$home$team$name, "-", gameData1$linescore$currentPeriodOrdinal, "Period", "-", gameData1$linescore$currentPeriodTimeRemaining, "Remaining"))
    p <- ggplotly(p, tooltip = "text")
  })
  
  output$PlaySummary <- renderDataTable({
    PBP <- plays()
    PBP <- flatten(PBP$plays$allPlays)
    w <- data.frame(PBP$about.periodTimeRemaining, PBP$team.triCode, PBP$result.event, PBP$result.description)
    w<-w[dim(w)[1]:1,]
    w <- rename(w, c("PBP.about.periodTimeRemaining" = "Period Time", "PBP.team.triCode" = "Team", "PBP.result.event" = "Event", "PBP.result.description" = "Description"))
    x <- datatable(w, options = list(dom = 'tp'))
    x
  })
  
  output$TeamStats <- renderTable({
    PBP <- plays()
    PBP <- flatten(PBP$plays$allPlays)
    PBP <- PBP[!is.na(PBP$team.triCode),]
    PBP$result.event <- factor(PBP$result.event, levels = c("Goal", "Shot", "Penalty", "Hit", "Faceoff", "Blocked Shot", "Missed Shot", "Takeaway", "Giveaway"))
    teams <- unique(PBP$team.triCode)
    PBPTeam1 <- PBP[PBP$team.triCode == teams[[1]],]
    PBPTeam2 <- PBP[PBP$team.triCode == teams[[2]],]
    Team1Stats <- table(PBPTeam1$result.event)
    Team2Stats <- table(PBPTeam2$result.event)
    x <- data.frame(Team1Stats, Team2Stats)
    x$Freq[[2]] <- x$Freq[[1]] + x$Freq[[2]]
    x$Freq.1[[2]] <- x$Freq.1[[1]] + x$Freq.1[[2]]
    y <- data.frame(x$Var1, x$Freq, x$Freq.1)
    y <- plyr::rename(y, c("x.Var1" = "Stat", "x.Freq" = teams[[1]], "x.Freq.1" = teams[[2]]))
    y
  })
})
