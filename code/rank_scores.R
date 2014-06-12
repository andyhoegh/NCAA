setwd('/Users/rlucas7/Dropbox/NCAA/preds')
n<-length(list.files()) #434 teams 


head(leaderboard)
names(leaderboard)
str(leaderboard)
### temp dataframe with the preds submissions and various scores
leaderboard[370:434,]<-NA
names(leaderboard)[1]<-'userid'
csv_names<-list.files()
for(i in 1:n){
leaderboard$userid[i]<-strsplit(csv_names[i],'_')[[1]][2]
}

score_ranks<-data.frame(leaderboard$userid, leaderboard$Score, leaderboard$Score+1,1.0,1.0,1.0)
names(score_ranks)[3]<-'score0'
names(score_ranks)[4]<-'score1'
names(score_ranks)[5]<-'score2'
names(score_ranks)[6]<-'score3'

### get only the games that happened
soln<-read.csv('/Users/rlucas7/NCAA/data/solution.csv')
logic<-soln$pred != -1
truth<-soln[logic,]

ties<-read.csv('/Users/rlucas7/NCAA/data/tourn_2014_results.csv')
head(ties)
### scoring functions ============================== 
scoring<-function(some1=truth, some2=pred){
	l<-length(some1)
	scores<-matrix(NA, nrow=l, ncol=1)
	for(i in 1:l){
		scores[i]<-some1[i]*log(some2[i]) + (1-some1[i])*log(1-some2[i])
	}
	return(sum(scores)/l)
}
scoring_alt1<-function(some1=truth, some2=pred){
	l<-length(some1)
	scores<-matrix(NA, nrow=l, ncol=1)
	for(i in 1:l){
		scores[i]<-some1[i]*log(2*abs(some2[i])) + (1-some1[i])*log(1-2*abs(.5-some2[i]) )
	}
	return(sum(scores)/l)
}
scoring_alt2<-function(some1=truth, some2=pred){
	l<-length(some1)
	scores<-matrix(NA, nrow=l, ncol=1)
	for(i in 1:l){
		scores[i]<-some1[i]*log(abs(some2[i])) + (1-some1[i])*log(1-abs(.5-some2[i]) )
	}
	return(sum(scores)/l)
}
scoring_alt3<-function(some1=truth, some2=pred, some3=ties){
	l<-length(some1)
	scores<-matrix(NA, nrow=l, ncol=1)
	for(i in 1:l){
		if(some3[i]==0){
			scores[i]<-some1[i]*log(some2[i]) + (1-some1[i])*log(1-some2[i])
		}else{
			if(some2[i]==0.5){scores[i]<-0}else{scores[i]<-abs(.5-some1[i])*log(abs(.5-some2[i]) ) }
			}
	}
	return(sum(scores)/l)
}


### end scoring functions ==============================

### waiting for an email from Will C. about matching teamids with userids. Need a table. 



for(i in 1:n){
pred<-read.csv(file=csv_names[i])
pred<-pred[logic,]
m<-dim(pred)[1]

pred[pred[,2] <=10^(-15),2]<-10^(-13)
pred[pred[,2] >= 1-10^(-15),2]<-1-10^(-13)
score_ranks$score0[i]<-	scoring(some1=as.numeric(truth[,2]),some2=pred[,2])
score_ranks$score1[i]<-	scoring_alt1(some1=as.numeric(truth[,2]),some2=pred[,2])
score_ranks$score2[i]<-	scoring_alt2(some1=as.numeric(truth[,2]),some2=pred[,2])
score_ranks$score3[i]<-	scoring_alt3(some1=as.numeric(truth[,2]),some2=pred[,2],some3=ties$OT)

}

#score_ranks$score0[366]<-0
setwd('/Users/rlucas7/NCAA/figures/')
pdf(file='prelim_rank_plot.pdf')
plot(1:433,score_ranks$score0[1:433],ylim=c(-2.8,0.2),xlab='contestant', ylab='score')
points(1:433,score_ranks$score1[1:433], col='red')
points(1:433,score_ranks$score2[1:433],col='blue')
points(1:433,score_ranks$score3[1:433],col='green', pch=2)
dev.off()


# now that we have the scores for each individual contestant lets match them up with the teams
# so that we can compare the leaderboard.

setwd('/Users/rlucas7/NCAA/data/')
list.files()
tut<-read.csv(file='QueryResults.csv')
leaderboard<-read.csv(file='march-machine-learning-mania_public_leaderboard.csv')
head(tut)
unique(tut$TeamName)
names(leaderboard)[3]<-'Score0'
names(leaderboard)[4]<-'Score1'
head(leaderboard)
str(leaderboard)
leaderboard$Score0<-leaderboard$Score1
leaderboard$Score2<-leaderboard$Score1
leaderboard$Score3<-leaderboard$Score1
leaderboard<-leaderboard[-332,]
leaderboard

setwd('/Users/rlucas7/Dropbox/NCAA/preds')
for(i in 1:368){
pred<-read.csv(file=csv_names[i])
pred<-pred[logic,]
m<-dim(pred)[1]

pred[pred[,2] <=10^(-15),2]<-10^(-13)
pred[pred[,2] >= 1-10^(-15),2]<-1-10^(-13)
leaderboard$Score0[i]<-	scoring(some1=as.numeric(truth[,2]),some2=pred[,2])
leaderboard$Score1[i]<-	scoring_alt1(some1=as.numeric(truth[,2]),some2=pred[,2])
leaderboard$Score2[i]<-	scoring_alt2(some1=as.numeric(truth[,2]),some2=pred[,2])
leaderboard$Score3[i]<-	scoring_alt3(some1=as.numeric(truth[,2]),some2=pred[,2],some3=ties$OT)

}
setwd('/Users/rlucas7/NCAA/data/')
write.csv(leaderboard, file='alternative_leaderboard.csv')

names(leaderboard)
alt_leader<-data.frame(unique(leaderboard$TeamName))
alt_leader$TeamName<-unique(leaderboard$TeamName)
names(alt_leader)[1]<-'teamname'
alt_leader$Score0<-10
alt_leader$Score1<-10
alt_leader$Score2<-10
alt_leader$Score3<-10
head(alt_leader)
dim(alt_leader)
head(leaderboard)
n<-length(unique(leaderboard$TeamName))
teams<-unique(leaderboard$TeamName)
for(i in 1:n){
 logic<-leaderboard$TeamName %in% 	teams[i] 
alt_leader$Score0[i]<- min(leaderboard[logic,3])
alt_leader$Score1[i]<- min(leaderboard[logic,4])
alt_leader$Score2[i]<- min(leaderboard[logic,5])
alt_leader$Score3[i]<- min(leaderboard[logic,6])
	}
	
setwd('/Users/rlucas7/NCAA/figures/')
pdf(file='team_rank_plot.pdf')
par(mar=c(15, 4, 4, 1),xaxs='i')
plot(alt_leader$TeamName[1:n],alt_leader$Score0[1:n],ylim=c(-2.8,0.2),xlab='', ylab='score',las=2,)
points(1:n,alt_leader$Score1[1:n], col='red')
points(1:n,alt_leader$Score2[1:n],col='blue')
points(1:n,alt_leader$Score3[1:n],col='green', pch=2)
dev.off()
system('open team_rank_plot.pdf')
