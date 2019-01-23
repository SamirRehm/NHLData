## Build hockey features
library(data.table)
library(xgboost)
require(xgboost)
library(Matrix)
library(jsonlite)
library(dplyr)
library(tidyr)
gameFeatures <- c()
gameFeaturesTest <- c()
for(i in 1:400) {
  gameFeatures[[i]] = getFeaturesForGame(100+i)
}
for(i in 1:200) {
  gameFeaturesTest[[i]] = getFeaturesForGame(500+i)
}

train <- rbindlist(gameFeatures)
train = train %>% drop_na()
dim(train)
sparse_train = sparse.model.matrix(goal ~ ., data = train)[,-1]
response_train = train$goal

test <- rbindlist(gameFeaturesTest)
test = test %>% drop_na()
sparse_test = sparse.model.matrix(goal ~ ., data = test)[,-1]
response_test = test$goal

dtrain <- xgb.DMatrix(data = sparse_train, label=response_train)
dtest <- xgb.DMatrix(data = sparse_test, label=response_test)

watchlist <- list(train=dtrain, test=dtest)

model <- xgb.train(data = dtrain,
                 watchlist = watchlist,
                  objective = "binary:logistic",
                  max.depth = 4, eta = 1,
                  nrounds=120, verbose=0
)
xgb.importance(feature_names = sparse_train@Dimnames[[2]], model = model)
y_pred <- predict(model, sparse_test)
compare = data.frame(y_pred, response_test)
mean(y_pred[response_test == 1])
sum(y_pred)
sum(response_test)

w <- tuneHyperParameters(dtrain, sparse_test, response_test, 4, 1)
w$numIters <- 91:130

tuneHyperParameters <- function(trainingSet, validationSet, validationResponse, depth, threshold) {
  meanZero <- c()
  meanOne <- c()
  responseSum <- c()
  iters <- c()
  currThreshold <- 1
  for(i in 1:10) {
  val <- 100+10*i
  model <- xgb.train(data = trainingSet,
                     objective = "binary:logistic",
                     max.depth = depth, eta = 1,
                     nrounds=ceiling(val), verbose=0
  )
  y_pred <- predict(model, validationSet)
  meanZero[[i]] <- mean(y_pred[validationResponse == 0])
  meanOne[[i]] <- mean(y_pred[validationResponse == 1])
  responseSum[[i]] <- sum(y_pred)
  iters[[i]] <- val
  }
  return(data.frame(iters, meanZero, meanOne, responseSum))
} 

getFeaturesForGame <- function(gameID) {
  tryCatch({
  game <- jsonlite::fromJSON(rawToChar(httr::GET(url = paste("http://statsapi.web.nhl.com/api/v1/game/", "2017020", gameID, "/feed/live", sep = ""))$content))
  playByPlay <- game$liveData$plays$allPlays
  playByPlayFiltered <- flatten(playByPlay[(playByPlay$result$event %in% c("Goal","Shot") & playByPlay$about$period <= 4),])
  playByPlayFiltered$result.emptyNet[is.na(playByPlayFiltered$result.emptyNet)] = FALSE
  playByPlayFiltered <- playByPlayFiltered[(playByPlayFiltered$result.emptyNet == FALSE),]
  plays <- select(playByPlayFiltered, result.event, about.periodTimeRemaining, players, result.secondaryType, coordinates.x, coordinates.y, team.triCode, about.period)
  homeCode <- game$gameData$teams$home$triCode
  plays$home = plays$team.triCode == homeCode
  plays$attackingGoal = ifelse( (plays$about.period %% 2 == 1), 
                                ifelse(plays$home, 89, -89), ifelse(plays$home, -89, 89))
  plays$goalDistance = sqrt( (plays$coordinates.x - plays$attackingGoal)^2 + plays$coordinates.y^2 )
  plays$angle = abs(atan(plays$coordinates.y/abs(plays$attackingGoal - plays$coordinates.x)))
  plays$goal = ifelse(plays$result.event == 'Goal', 1, 0)
  plays$gameSeconds = 1200*(plays$about.period-1) + (1200-toSeconds(plays$about.periodTimeRemaining))
  plays = plays %>%
    group_by(home) %>%
    mutate(secondsSince = gameSeconds - lag(gameSeconds)) %>%
    mutate(previousX = lag(coordinates.x)) %>%
    mutate(previousY = lag(coordinates.y))
  plays$secondsSince[is.na(plays$secondsSince)] <- 9999
  plays$distanceFromPrevious = sqrt((plays$previousX-plays$coordinates.x)^2 + (plays$previousY-plays$coordinates.y)^2)
  features = select(plays, result.secondaryType, previousX, previousY, distanceFromPrevious, about.period, goalDistance, angle, secondsSince, coordinates.x, coordinates.y, goal)
  return(features)
  }, error=function(err) {print(err)
    return(NULL)})
}
