###########################################################
# Input Data & Preliminary Analysis for NCAA challenge
# 2.20.2014
# Andy Hoegh
###########################################################
setwd("~/NCAA")
library(mnormt) # rmnorm
library(pscl) # rigamma
################################################################
# FUNCTIONS
################################################################
get.sag <- function(team, daynum, season){
  ranks <- dat.sag[dat.sag$season == season & dat.sag$rating_day_num <= daynum & dat.sag$team == team,]
  if (nrow(ranks) > 1) {
    ranks <- ranks[ranks$rating_day_num == max(ranks$rating_day_num),]
  }
  if (nrow(ranks) == 0 ){
    return(c(NA,NA))
  }
  return(c(ranks$orank,ranks$rating))
}
find.ranks <- function(wteam,lteam,wrating,lrating){
  if (wrating < lrating) {
    return(wteam)
  } else {
    return(lteam)
  }
}

flip.prob <- function(prob,indicator){
  if (indicator == TRUE){
    return(1 - prob)
  } else {
    return(prob)
  }
}

################################################################
# Create a File with Static Team Level Characteristics
################################################################


################################################################
# Working on simple models to predict Score Diff
################################################################
bball <- read.csv('~/NCAA/regular_season_results.csv')
bball$score_diff <- bball$wscore - bball$lscore
keep.seasons <- c('N','O','P','Q','R')
bball.small <- bball[bball$season %in% keep.seasons,]

# ONLY INCLUDE GAMES AFTER INITIAL RANKINGS RELEASED 
win.sag <- t(mapply(get.sag, team = bball.small$wteam,daynum = bball.small$daynum,season=as.character(bball.small$season)))
lose.sag <- t(mapply(get.sag, team = bball.small$lteam,daynum = bball.small$daynum,season=as.character(bball.small$season)))
colnames(win.sag) <-c('wrank','wrating') 
colnames(lose.sag) <-c('lrank','lrating')
comb <- data.frame(bball.small,(win.sag),(lose.sag))
comb$rating_diff <- abs(comb$wrating - comb$lrating)
comb$rank_diff <- abs(comb$wrank - comb$lrank)
comb$home <- as.numeric(comb$wrank < comb$lrank & comb$wloc == 'H' | comb$lrank < comb$wrank & comb$wloc=='A') # to know if higher rated team is at home
comb$away <- as.numeric(comb$wrank < comb$lrank & comb$wloc == 'A' | comb$lrank < comb$wrank & comb$wloc=='H') # to know if higher rated team is at home
comb$neutral <- as.numeric(comb$wloc == 'N') 

mult <- as.numeric(comb$wrank < comb$lrank) # does higher rated team win?
mult[mult==0] <- -1
comb$score_diff <- comb$score_diff * mult
comb <- comb[!is.na(comb$wrank),]


# Gibbs Sampler 
#p(\beta) = 1 & p(\sigma2) = 1/sigma2
num.iter <- 1000
X <- matrix(cbind(comb$rating_diff,comb$home, comb$away,comb$neutral),ncol=4)
y <- comb$score_diff
beta.vals <- matrix(0, ncol=ncol(X), nrow=num.iter)
sigma2.vals <- rep(1,num.iter)
n <- length(y)
for (i in 2:num.iter){
  
beta.vals[i,] <-rmnorm(1,solve(t(X) %*% X) %*% t(X) %*% y, sigma.vals[i-1] * solve(t(X) %*% X))

sigma2.vals[i] <- rigamma(1,n/2, .5 * t(y - X %*% beta.vals[i,]) %*% (y - X %*% beta.vals[i,]))
}

plot(sigma2.vals,type='l')
mean(sigma2.vals)
plot(beta.vals[,1],type='l')
plot(beta.vals[,2],type='l')
plot(beta.vals[,3],type='l')
plot(beta.vals[,4],type='l')
apply(beta.vals,2,mean)

################################################################
# Posterior Predictive Distribution for actual tourney_games
################################################################
tourney <- read.csv('tourney_results.csv')
tourney <- tourney[tourney$season %in% keep.seasons,]
win.sag.t <- t(mapply(get.sag, team = tourney$wteam,daynum = tourney$daynum,season=as.character(tourney$season)))
lose.sag.t <- t(mapply(get.sag, team = tourney$lteam,daynum = tourney$daynum,season=as.character(tourney$season)))
colnames(win.sag.t) <- c('wrank','wrating')
colnames(lose.sag.t) <- c('lrank','lrating')
tourney <- data.frame(tourney,(win.sag.t),(lose.sag.t))
tourney$rating_diff <- abs(tourney$wrating - tourney$lrating)
tourney$rank_diff <- abs(tourney$wrank - tourney$lrank)
mult <- as.numeric(tourney$wrank < tourney$lrank) # does higher rated team win?
mult[mult==0] <- -1
tourney$score_diff <- (tourney$wscore - tourney$lscore) * mult

num.games <- nrow(tourney)
games <- matrix(0,nrow=(num.iter-1), ncol = num.games)
for (i in 2:num.iter){
  games[(i-1),] <- tourney$rating_diff * beta.vals[i,1] + rep(beta.vals[i,4],num.games) + rnorm(num.games,0,sqrt(sigma2.vals[i]))
}

probs <- apply(games > 0,2,mean)
# Have prob higher rated team wins, need prob lower numbered team wins
team.1 <- mapply(min,tourney$wteam,tourney$lteam)
team.2 <- mapply(max,tourney$wteam,tourney$lteam)


low.rank <- mapply(find.ranks,tourney$wteam,tourney$lteam,tourney$wrating,tourney$lrating)
indicator <- team.1 == low.rank
probs.tmp <- mapply(flip.prob, probs,indicator)

test.out <- data.frame(tourney,paste(tourney$season,team.1,team.2, sep='_'),probs,probs.tmp)
colnames(test.out)[15] <- 'id'
test.out.sort <- test.out[order(test.out$id),]
sample.submission <- data.frame(paste(tourney$season,team.1,team.2, sep='_'),probs.tmp)
colnames(sample.submission) = c('id','pred')

sample.submission.sort <- sample.submission[order(sample.submission$id),]


submission <- read.csv('sample_submission.csv') #includes seasons N,O,P,Q,R
submission$pred <- .5

submission[submission$id %in% sample.submission$id,2] <- sample.submission.sort$pred

Score.NCAA(tourney_results,predict.years,submission)


#write.csv(submission,'HokieStat_submission1.csv')


