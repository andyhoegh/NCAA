###############################################
# Scoring Function for NCAA challenge
# 2.17.2014
# Andy Hoegh
###############################################

###############################################
# Functions
###############################################
create.game <- function(season,wteam,lteam){
  team1 <- sort(c(wteam,lteam))[1]
  team2 <- sort(c(wteam,lteam))[2]
  possible.game <- paste(season,team1,team2,sep='_')
  if (team1 == wteam) outcome=1
  else outcome=0
  return(c(possible.game,outcome))
}

log.loss <- function(outcome,pred){
  return(as.numeric(outcome) * log(pred) + (1-as.numeric(outcome)) * log(1-pred))
}

Score.NCAA <- function(tourney_results,predict.years,sample.submission){
  tourney_results <- tourney_results[!(tourney_results$daynum %in% c(134,135)),] # exclude play in
  tourney_results_predyears <- tourney_results[tourney_results$season %in% predict.years,]
  
  ###############################################
  # Identify Realized Matchups
  ###############################################
  possible.games <- NULL
  actual.games <- t(mapply(create.game,season=tourney_results_predyears$season,wteam=tourney_results_predyears$wteam,
                           lteam=tourney_results_predyears$lteam))
  actual.games <- data.frame(matrix(actual.games,ncol=2))
  colnames(actual.games)<- c('id','outcome')
  ###############################################
  # Calculate Loss
  ###############################################
  keep.games <- sample.submission[sample.submission$id %in% actual.games$id,]
  game.info <- merge(keep.games,actual.games)
  loss <-mapply(log.loss,outcome=as.character(game.info$outcome),pred=game.info$pred)
  return(mean(-loss))
}

###############################################
# Read in Data
###############################################
setwd("~/NCAA")
submission <- read.csv('sample_submission.csv') #includes seasons N,O,P,Q,R

###############################################
# Identify Possible Matchups
###############################################
predict.years <-c('N','O','P','Q','R')
tourney_results <- read.csv('tourney_results.csv')
tourney_results.tmp <- tourney_results[!(tourney_results$daynum %in% c(134,135)),] # exclude play in
tourney_results_predyears <- tourney_results[tourney_results$season %in% predict.years,]

all_matchups <- NULL
for (k in 1:length(predict.years)){
  active.year <- tourney_results_predyears[tourney_results_predyears$season == predict.years[k] ,]
  teams <- sort(unique(c(tourney_results_predyears$wteam,tourney_results_predyears$lteam)))
  for (i in 1:(length(teams)-1)){
    for (j in (i+1):length(teams)){
      all_matchups <- c(all_matchups, paste(predict.years[k],teams[i],teams[j],sep='_'))
    }
  }  
}

head(all_matchups)
###############################################
# Input Predictions Here currently using naive
###############################################
pred <- rep(.5,length(all_matchups))
sample.submission <- data.frame(all_matchups,pred)
colnames(sample.submission) = c('id','pred')

###############################################
# Score Predictions Here currently using naive
###############################################
Score.NCAA(tourney_results,predict.years,submission)
submission <- read.csv('HokieStat_submission1.csv')

submission$pred <- 1-submission$pred
head(submission)