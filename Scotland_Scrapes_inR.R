library(RCurl)

### 1) First task is to get all of the web links we will need ##
base_url<-"http://msn.foxsports.com/collegebasketball/teams/"
base_html<-getURLContent(base_url)[[1]]
links<-strsplit(base_html,"a href=")[[1]]

links<-links[84:785] #only half of these are relevant
links <- links[seq(1, length(links), 2)]

#parse links… working here
parse_url<-function(s) {
  root<-"http://msn.foxsports.com"  
  u_split0<-strsplit(s,"\"")[[1]][2]	
  u_split1<-strsplit(u_split0,"team/")[[1]][2] 	
  teamName<-strsplit(u_split1,"/")[[1]][1] 	
  return(paste(root,u_split0,"/?q=",teamName,sep=""))
}

URLs<-unlist(lapply(links,parse_url))


#for each team, there are three links that need to be accessed: 
#stats, roster, and schedule

get_stats_url<-function(s) {
  u_split1<-strsplit(s,"-basketball")[[1]][1]
  u_split2<-strsplit(s,"-basketball")[[1]][2]
  
  teamName<-strsplit(u_split2,"?=")[[1]][2]
  
  
  return(paste(u_split1,"-basketball/stats",u_split2,"-basketball",sep=""))
}


get_roster_url<-function(s) {
  u_split1<-strsplit(s,"-basketball")[[1]][1]
  u_split2<-strsplit(s,"-basketball")[[1]][2]
  
  teamName<-strsplit(u_split2,"?=")[[1]][2]
  
  
  return(paste(u_split1,"-basketball/roster",u_split2,"-basketball",sep=""))
}

get_schedule_url<-function(s) {
  u_split1<-strsplit(s,"-basketball")[[1]][1]
  u_split2<-strsplit(s,"-basketball")[[1]][2]
  
  teamName<-strsplit(u_split2,"?=")[[1]][2]
  
  
  return(paste(u_split1,"-basketball/schedule",u_split2,"-basketball",sep=""))
}

statsURL<-unlist(lapply(URLs,get_stats_url))
rosterURL<-unlist(lapply(URLs,get_roster_url))
scheduleURL<-unlist(lapply(URLs,get_schedule_url))

# This will be a high dimensional list with all of the data, you can then manipulate as you like
get_structs<-function(u) {
  raw_data<-getURLContent(u)
  return(raw_data)
}

#lapply the function above to the statsURL, rosterURL, and scheduleURL. 
#This will accumulate a ton of data that needs to be parsed out