setwd('/Users/rlucas7/Dropbox/NCAA/preds')
n<-length(list.files()) #434 teams 

leaderboard<-read.csv(file='/Users/rlucas7/NCAA/data/march-machine-learning-mania_public_leaderboard.csv',header=TRUE)


head(leaderboard)
names(leaderboard)
str(leaderboard)
### temp dataframe with the preds submissions and various scores
score_ranks<-data.frame(leaderboard$TeamId, leaderboard$Score, leaderboard$Score+1,1.0,1.0,1.0)
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
			scores[i]<-abs(.5-some1[i])*log(abs(.5-some2[i]) ) 
			}
	}
	return(sum(scores)/l)
}


### end scoring functions ==============================

### waiting for an email from Will C. about matching teamids with userids. Need a table. 

csv_names<-list.files()
read.csv(file='tourn_2014_results.csv')
for(i in 1:n){
pred<-read.csv(file=csv_names[i])
pred<-pred[logic,]
m<-dim(pred)[1]

pred[,2]

score_ranks$score0[i]<-	scoring(some1=as.numeric(truth[,2]),some2=pred[,2])
score_ranks$score1[i]<-	scoring_alt1(some1=as.numeric(truth[,2]),some2=pred[,2])
score_ranks$score2[i]<-	scoring_alt2(some1=as.numeric(truth[,2]),some2=pred[,2])
score_ranks$score3[i]<-	scoring_alt3(some1=as.numeric(truth[,2]),some2=pred[,2],some3=ties$OT)

}

#score_ranks$score0[366]<-0

pdf(file='prelim_rank_plot.pdf')
plot(1:330,score_ranks$score0[1:330],ylim=c(-2.8,0.2),xlab='contestant', ylab='score')
points(1:330,score_ranks$score1[1:330], col='red')
points(1:330,score_ranks$score2[1:330],col='blue')
points(1:330,score_ranks$score3[1:330],col='green', pch=2)
dev.off()
