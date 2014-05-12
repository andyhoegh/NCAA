source("scoring_code.R")

# This gets the win rate for all teams over all given seasons

id = 501:856
nteams = length(id)
regular_season_results = read.csv("~/regular_season_results.csv")

nwins = numeric(length(id))
nlosses = numeric(length(id))

for(i in 1:nteams){
  nwins[i] = sum(regular_season_results$wteam == id[i])
  nlosses[i] = sum(regular_season_results$lteam == id[i])
}

propwins = nwins / (nwins + nlosses)
propwins[is.nan(propwins)] = .5

id_propwins = data.frame(id, propwins)

# This is a naive predictor. The probability a team wins a matchup is its win % divided by its win % + its opponent's


predict.years <-c('N','O','P','Q','R')
tourney_results <- read.csv('C:\\Users\\Ian\\Desktop\\Research\\NCAA\\data\\tourney_results.csv')
tourney_results.tmp <- tourney_results[!(tourney_results$daynum %in% c(134,135)),] # exclude play in
tourney_results_predyears <- tourney_results[tourney_results$season %in% predict.years,]

all_matchups <- NULL
for (k in 1:length(predict.years)){
  active.year <- tourney_results_predyears[tourney_results_predyears$season == predict.years[k] ,]
  teams <- sort(unique(c(active.year$wteam,active.year$lteam)))
  for (i in 1:(length(teams)-1)){
    for (j in (i+1):length(teams)){
      all_matchups <- c(all_matchups, paste(predict.years[k],teams[i],teams[j],sep='_'))
    }
  }  
}

pred = numeric(length(all_matchups))
for(i in 1:length(all_matchups)){
  team1 = as.numeric(strsplit(all_matchups[i], "_")[[1]][2])
  team2 = as.numeric(strsplit(all_matchups[i], "_")[[1]][3])
  prop1 = id_propwins[id_propwins$id == team1, 2]
  prop2 = id_propwins[id_propwins$id == team2, 2]
  pred = prop2 / (prop1 + prop2)
}

sample.submission <- data.frame(all_matchups,pred)
colnames(sample.submission) = c('id','pred')
Score.NCAA(tourney_results,predict.years,sample.submission)
