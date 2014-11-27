# NCAA Decision Theory

results <- read.csv('/Users/Marcos/Desktop/NCAA/NCAA Decision Theory/March Madness Results.csv')
team_names <- read.csv('/Users/Marcos/Desktop/NCAA/teams.csv')
arb_prob <- read.csv('/Users/Marcos/Desktop/NCAA/NCAA Decision Theory/preds/2014_175983_638707_4.zip.csv')

scoring <- function(probs,true_bracket=results,name=team_names,round=6,bound=1e-15,individual=FALSE){
	score <- 0
	if(class(probs[,1])=='numeric') probs <- probs[,2:1]
	if(individual) individual_scores <- vector('numeric',sum(2^(6-1:round)))
	k <- 1
	
	for(i in 1:round){
		
		for(j in 1:(2^(6-i))){
			
			team_1 <- name[which(as.character(name[,2])==as.character(true_bracket[true_bracket$Round==i,1])[j]),1]
			team_2 <- name[which(as.character(name[,2])==as.character(true_bracket[true_bracket$Round==i,2])[j]),1]
			
			tmp_name_right <- paste('S',team_1,team_2,sep='_')
			tmp_name_wrong <- paste('S',team_2,team_1,sep='_')
			
			if(sum(probs[,1]==tmp_name_right)==1) p <- probs[which(probs[,1]==tmp_name_right),2] else p <- 1-probs[which(probs[,1]==tmp_name_wrong),2]
		
		if(p<min(bound,1-bound)) p <- min(bound,1-bound)
		if(p>max(bound,1-bound)) p <- max(bound,1-bound)
		
		score <- score-log(p)
		if(individual){
			individual_scores[k] <- p
			k <- k+1
			}
		}
		
		
		
	}
	if(individual) return(individual_scores)
	return(score/(sum(2^(6-1:round))))
}
results[which(!results$Losing.Team%in%team_names[,2]),2]
results[which(!results[,1]%in%results[,2]),]

arb_prob[,2] <- 0

scoring(arb_prob,round=6,individual=FALSE,bound=1e-15)

x <- 10^seq(-19,-17,by=0.01)
plot(19.18821-(tmp <- sapply(x,FUN=function(y) scoring(arb_prob,round=6,bound=y))))


# Calculate all team scores

filenames <- list.files('/Users/Marcos/Desktop/NCAA/NCAA Decision Theory/preds/')
probabilities <- 1:(length(filenames)*63)
scores <- 1:length(filenames)
for(i in 1:length(filenames)){
	
	path <- paste('/Users/Marcos/Desktop/NCAA/NCAA Decision Theory/preds/',filenames[i],sep='')
	probs <- read.csv(path)
	if(class(probs[,1])=='numeric') probs <- probs[,2:1]
	probabilities[1:63+(i-1)*63] <- scoring(probs,round=6,individual=TRUE)
	scores[i] <- scoring(probs,round=6,individual=FALSE)
}

ranks <- rep(rank(scores,ties.method='first'),each=63)
boxplot(probabilities~orders,xlab='Ranking',ylab='Predicted Probabilities for Observed Outcomes')
boxplot(probabilities[orders<=100]~orders[orders<=100],xlab='Ranking',ylab='Predicted Probabilities for Observed Outcomes',col=c(rep('white',77),'red',rep('white',100-78)))
stripchart(probabilities~orders,vertical=TRUE)


par(mar=c(5,4,0.1,0.1),las=1)
top <- 200
inds <- head(order(scores,decreasing=TRUE),top)
inds <- c(sapply(inds,FUN=function(x) 1:63+(x-1)*63))
meds <- aggregate(probabilities[inds],list(rep(1:63,times=top)),median)[,2]
labs <- sapply(1:63,FUN=function(x) paste(results[x,1],' / ',results[x,2],' (',results[x,3],')',sep=''))
boxplot(probabilities[inds]~rep(1:63,times=length(filenames))[inds],horizontal=TRUE,axes=TRUE,col=c('white','grey')[(meds<0.5)+1],outline=FALSE,xlab='Predicted Probability of True Game Outcome',cex.axis=0.7,ylab='Game Number')
axis(2,at=1:63,labels=1:63)
axis(1)
abline(v=0.5,lty=2,lwd=2)
probs <- read.csv('/Users/Marcos/Downloads/nn_predicts.csv')
nam <- apply(probs,1,FUN=function(x) paste('S',x[5],x[6],sep='_'))
arb_prob <- read.csv('/Users/Marcos/Desktop/NCAA/NCAA Decision Theory/preds/2014_175983_638707_4.zip.csv')
for(i in 1:length(nam)){ind <- which(arb_prob[,1]==nam[i]); arb_prob[ind,2] <- probs[i,4]}
temp_probs <- scoring(arb_prob,individual=TRUE)
points(temp_probs,1:63,pch=16,col='white')
points(temp_probs,1:63)


hist(head(sort(scores),400),add=FALSE,freq=FALSE,breaks=20,main='',xlab='Kaggle Score')
lines(density(head(sort(scores),400)),lwd=2)


par(mar=c(2,14,0.1,0.1),las=1)
boxplot(probabilities[inds][rep(1:63,times=length(filenames))[inds]>32]~rep(1:63,times=length(filenames))[inds][rep(1:63,times=length(filenames))[inds]>32],horizontal=TRUE,axes=FALSE,col=c('white','grey')[(meds[33:63]<0.5)+1],outline=FALSE)
axis(2,at=1:31,labels=labs[33:63])
axis(1)
abline(v=0.5,lty=2,lwd=2)


par(mar=c(0.1,14,0.1,0.1),las=1)
boxplot(probabilities~rep(order(scores,decreasing=FALSE),each=63),horizontal=TRUE,axes=TRUE)
axis(2,at=1:63,labels=sapply(1:63,FUN=function(x) paste(results[x,1],results[x,2],sep=' / ')))
abline(v=0.5,lty=2,lwd=2)


library(MASS)
?fitdistr
(fit <- fitdistr(probabilities[probabilities!=0.5&probabilities>0.01&probabilities<0.99],'beta',start=list(shape1=0.5,shape2=0.01)))
qqplot(probabilities[probabilities!=0.5&probabilities>0.01&probabilities<0.99],rbeta(sum(probabilities!=0.5&probabilities>0.01&probabilities<0.99),shape1=fit$estimate[1],shape2=fit$estimate[2]))
fitdistr(head(sort(scores),100),'gamma')$loglik
unlist(fitdistr(scores,'gamma'))[1:2]
qqplot(scores,rgamma(length(scores),unlist(fitdistr(scores,'gamma'))[1],unlist(fitdistr(scores,'gamma'))[2]))
qqplot(scores,exp(rnorm(length(scores),unlist(fitdistr(scores,'lognormal'))[1],unlist(fitdistr(scores,'lognormal'))[2])))

lognorm_params <- unlist(fitdistr(-log(probabilities[probabilities>exp(-1)&probabilities<1]),'lognormal'))[1:2]
qqplot(-log(probabilities[probabilities>exp(-1)&probabilities<1]),exp(rnorm(sum(probabilities>exp(-1)&probabilities<1),mean=lognorm_params[1],sd=lognorm_params[2])))

# Lognormal distribution of scores and loss function

lognorm_cdf <- function(x,mean=lognorm_params[1],sd=lognorm_params[2],...){
	
	return(pnorm(log(x),mean,sd))
	
}

loss <- function(prob,sub,cdf=pbeta,n=1,...){
	
	return(prob*(cdf(sub,...))^n+(1-prob)*(cdf(1-sub,...))^n)
	
}
x <- seq(0,1,by=0.001)
plot(x,loss(0.51,x,n=1,shape1=fit$estimate[1],shape2=fit$estimate[2]),type='l')
x[which.max(loss(0.2,x,shape1=fit$estimate[1],shape2=fit$estimate[2]))]

# shift function

shift <- function(p,alpha=0){
	
	if(alpha<=0){
		return((1+alpha)*p-alpha*0.5)
	} else if(p<0.5){
		return((1-alpha)*p)
	} else {
		return(alpha+(1-alpha)*p)
	}
	
}

alphas <- seq(-0.5,0.3,by=0.0005)
scoring(probs)
test_scores <- alphas*NA
best_alphas <- 1:length(filenames)*NA
best_scores <- 1:length(filenames)*NA
for(j in 1:length(filenames)){
	path <- paste('/Users/Marcos/Desktop/NCAA/NCAA Decision Theory/preds/',filenames[j],sep='')
	probs <- read.csv(path)
	if(class(probs[,1])=='numeric') probs <- probs[,2:1]
	
	for(i in 1:length(alphas)){
		
		new_probs <- probs
		new_probs[,2] <- sapply(new_probs[,2],shift,alphas[i])
		test_scores[i] <- scoring(new_probs)
		
		
	}
	best_alphas[j] <- alphas[which.min(test_scores)]
	best_scores[j] <- min(test_scores)
	print(j)
}
set.seed(123)
plot((best_alphas[scores<1]+rnorm(sum(scores<1),0,0.025)),best_scores[scores<1]-scores[scores<1],ylab='L(y,p\')-L(y,p)',xlab=expression(alpha),pch=16,cex=1)
points(best_alphas[17],best_scores[17]-scores[17],col='white',pch=15,cex=1.5)
points(best_alphas[17],best_scores[17]-scores[17],col='black',pch=12,cex=1.5)
best_one <- which.min(scores)
points(best_alphas[best_one],best_scores[best_one]-scores[best_one],col='white',pch=15,cex=1.5)
points(best_alphas[best_one],best_scores[best_one]-scores[best_one],col='black',pch=13,cex=1.5)

lines(lowess(best_scores[scores<1]-scores[scores<1]~best_alphas[scores<1]),col='red')