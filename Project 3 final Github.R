rm(list=ls())
#John Dobson

#Creating dataframe with Home/Away Team and score information

colcounts=c(11,28,2,29,3,19)
webdata="http://homepages.cae.wisc.edu/~dwilson/rsfc/history/howell/cf"
gamedata="gms.txt"

dates <- 1960:2010

df=data.frame("Date"=numeric(),"Away Team"=character(),"Away Score"=numeric(),"Home Team"=character(),"Home Score"=numeric(),"Location"=character(),"Seasons"=numeric())
year=numeric()
listbyseason<-list()
for (i in 1:51){
  a<- read.fwf(paste(webdata,dates[i],gamedata, sep = ""),colcounts)
  seasons<-dates[i]
  a<-cbind(a,seasons)
  listbyseason[[i]]<-a
}
colnames(df) <- c("Date","Away","AwayScore","Home","HomeScore","Location","Season") 


#Removing division 2 teams
for (i in 1:51) {
  listbyseason[[i]]$V2<-gsub(" ","", listbyseason[[i]]$V2)
  listbyseason[[i]]$V4<-gsub(" ","", listbyseason[[i]]$V4)
  listbyseason[[i]]$V4<-gsub("\\(","",listbyseason[[i]]$V4)
  listbyseason[[i]]$V4<-gsub("\\)","",listbyseason[[i]]$V4)
  listbyseason[[i]]$V2<-gsub("\\(","",listbyseason[[i]]$V2)
  listbyseason[[i]]$V2<-gsub("\\)","",listbyseason[[i]]$V2)
}
#Creating vector to signal non-div1 teams
for (i in 1:51){
  listbyseason[[i]]$div1<-0
}
#signaling non-div1 teams
for (i in 1:51) {
  for (j in 1:length(listbyseason[[i]]$V2)) {
    if((length(grep(listbyseason[[i]]$V2[j],listbyseason[[i]]$V2))+length(grep(listbyseason[[i]]$V2[j],listbyseason[[i]]$V4)))<6){
      listbyseason[[i]]$div1[j]<-1
    }
    if((length(grep(listbyseason[[i]]$V4[j],listbyseason[[i]]$V2))+length(grep(listbyseason[[i]]$V4[j],listbyseason[[i]]$V4)))<6){
      listbyseason[[i]]$div1[j]<-1
    }
  }
}


df<-listbyseason[[2]]
for (i in 3:51) {
  x<-listbyseason[[i]]
  df<-rbind(df,x)
}
x<-which(df$div1==0)
df<-df[x,]
colnames(df) <- c("Date","Away","AwayScore","Home","HomeScore","Location","Season","Div1") 

#Removing ties
Tie<-which(df$AwayScore==df$HomeScore)
df<-df[-c(Tie),]

Awaybyseason<-list()
Homebyseason<-list()
listofdf<- list()
listofdf2<-list()
#Creating  dataframes for each season
for (i in 2:51) {
  Awaybyseason[[i]]<-as.data.frame(table(df$Away[df$Season==dates[i]]))
  Homebyseason[[i]]<-as.data.frame(table(df$Home[df$Season==dates[i]]))
  mergeddf<-merge(Awaybyseason[[i]],Homebyseason[[i]],by.x = "Var1",by.y = "Var1")
  mergeddf$GP<-mergeddf$Freq.x+mergeddf$Freq.y
  mergeddf$Season<-dates[i]
  mergeddf$wins<-0
  mergeddf$losses<-0
  listofdf[[i]]<-mergeddf
}


df1<-df
#Wins & losses loops
for(i in 2:51){
  for (k in 1:length(listofdf[[i]]$Var1)){
    wincount = 0
    losscount = 0
    T<-listofdf[[i]]$Var1[k]
    for(j in 1:length(df1$Away[df1$Season==dates[i]])){
      if(df1$Away[df1$Season==dates[i]][j] == T){
        if(df1$AwayScore[df1$Season==dates[i]][j] < df1$HomeScore[df1$Season==dates[i]][j]){
          losscount = losscount + 1
        }
        if(df1$AwayScore[df1$Season==dates[i]][j] > df1$HomeScore[df1$Season==dates[i]][j]){
          wincount = wincount + 1
        }
      }
      if(df1$Home[df1$Season==dates[i]][j] == T){
        if(df1$HomeScore[df1$Season==dates[i]][j] < df1$AwayScore[df1$Season==dates[i]][j]){
          losscount = losscount + 1
        }
        if(df1$HomeScore[df1$Season==dates[i]][j] > df1$AwayScore[df1$Season==dates[i]][j]){
          wincount = wincount + 1
        }
      }
      losses = losscount
      listofdf[[i]]$losses[k] = losses
      wins = wincount
      listofdf[[i]]$wins[k] = wins
    }
  }
}



#Create Opponents vector
for (i in 2:51) {
  listofdf[[i]]$opponents<-list(c())
}


opponents<-list()

for (i in 2:51) {
  for (j in 1:length(listofdf[[i]]$Var1)) {
    opponents<-c()
    opponent<-numeric()
    T<-listofdf[[i]]$Var1[j]
    for (k in 1:length(df1$Away[df1$Season==dates[i]])) {
      if(df1$Away[df1$Season==dates[i]][k] == T){
        opponent<-which(listofdf[[i]]$Var1==df1$Home[df1$Season==dates[i]][k])
        opponents<-c(opponents,opponent)
      }
      if(df1$Home[df1$Season==dates[i]][k] == T){
        opponent<-which(listofdf[[i]]$Var1==df1$Away[df1$Season==dates[i]][k])
        opponents<-c(opponents,opponent)
      }
    }
    list(opponents)
    listofdf[[i]]$opponents[j]<-list(opponents)
  }
}


myreallybigdataframe<-listofdf[[1]]
for (i in 2:51) {
  x<-listofdf[[i]]
  myreallybigdataframe<-rbind(myreallybigdataframe,x)
}

myreallybigdataframe$GP<-myreallybigdataframe$wins + myreallybigdataframe$losses
NCAAdataframe<-myreallybigdataframe
x<-2:3
NCAAdataframe<-NCAAdataframe[,-x]
colnames(NCAAdataframe) <- c("Team","Games Played","Season","Wins","Losses","Opponents") 

save(NCAAdataframe,file = "NCAAdataframe.rda")

rm(list=ls())
load("NCAAdataframe.rda")
df<-NCAAdataframe
#Creating Colley Matrix Function


Colley<-function(x){
  Teams<-df$Team[df$Season==x]
  wins<-c(df$Wins[df$Season==x])
  losses<-c(df$Losses[df$Season==x])
  gamesplayed<-c(wins+losses)
  opponents<-c(df$Opponents[df$Season==x])
  a<-length(df$Team[df$Season==x])
  A<-matrix(nrow=a,ncol=a)
  
  for (i in 1:a){
    A[i,i]<-(gamesplayed[i]+2) 
  }
  
  for (i in 1:a){
    for (j in 1:a){
      if(i==j)
      {next}
      A[i,j]<--1*length(which(opponents[[j]]==i))
    }
  }
  
  
  b<-c(1:a)
  for (i in 1:a){
    b[i]<-(1+((wins[i]-losses[i]))/2)
  }
  
  RankingScore<-c(solve(A,b))
  Solution<-data.frame(Teams,RankingScore)
  transform.data.frame(Solution)
  Rankings<-Solution[order(-RankingScore),]
  View(Rankings)
}


