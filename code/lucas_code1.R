### this code does some stuff    ;) 

getwd()
setwd("/Users/rlucas7/NCAA")

soln<-read.csv('solution.csv')
pred1<-read.csv("/Users/rlucas7/Dropbox/NCAA/preds/308_165938_638241_4.zip.csv")

head(pred1)
head(soln)

logic<-soln$pred != -1
truth<-soln[logic,]
pred<-pred1[logic,]

scoring<-function(some1=truth, some2=pred){
	n<-dim(some1)[1]
	scores<-matrix(NA, nrow=n, ncol=1)
	for(i in 1:n){
		scores[i]<-some1[i,2]*log(some2[i,2]) + (1-some1[i,2])*log(1-some2[i,2])
	}
	return(sum(scores)/n)
}


### now some code to get all the names in the 'preds' folder 

write.csv(truth,file='tourn_2014_results.csv', row.names=FALSE)
getwd()
system('open tourn_2014_results.csv')

r_ot <-read.csv(file='tourn_2014_results.csv')

### make the loss 0.5 for the OT games ....
### what about if we vary from 0.1,0.9, by 0.1?
length(seq(0.01,0.99, by=0.01))
plot_mat<-matrix(NA, nrow=99,ncol=2)
plot_mat[,1]<-seq(0.01,0.99, by=0.01)

n<-dim(r_ot)[1]
beta_var<-seq(0.01,0.99, by=0.01)
for(j in 1:99){
	for(i in 1:n){
		if(r_ot[i,]$OT==1){
		r_ot[i,]$pred<-beta_var[j]
		}
	}
plot_mat[j,2]<-scoring(r_ot, pred) # scoring them at 0.5 

}

pdf(file='vary_scoring_beta.pdf')
plot(plot_mat[,1],plot_mat[,2],type='l',xlab='beta', ylab='score')
dev.off()
getwd()
system('open vary_scoring_beta.pdf')