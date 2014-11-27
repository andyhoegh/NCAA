results <- read.csv('/Users/Marcos/Desktop/NCAA/NCAA Decision Theory/March Madness Results.csv')
team_names <- read.csv('/Users/Marcos/Desktop/NCAA/teams.csv')

scoring <- function(probs,true_bracket=results,name=team_names,round=6,bound=1e-15){
	# probs is a csv file in the format that Kaggle accepts it
	
	score <- 0
	if(class(probs[,1])=='numeric') probs <- probs[,2:1]
	
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
		
		}
		
		
		
	}
	
	return(score/(sum(2^(6-1:round))))
}


