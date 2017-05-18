rm(list=ls())
df<-read.csv("pddata.csv",sep=",",head=TRUE)   
#where j is A or B and Aji is the propensity to play strategy j in round i,
#I is an indicator function equal to 1 if strategy j was played in round i − 1 and 0 otherwise, 
#G is the payoff received from playing strategy j in round i − 1 (more details below), 
#and d ∈ [0, 1] is a parameter to be estimated. The probability of choosing j in round i is:

#testing propensity
#Pa = exp(h Aa_i) / (exp(h Aa_i)+exp(h Ab_i))
#Pb = exp(h Ab_i) / (exp(h Aa_i)+exp(h Ab_i)) = 1-Pa
#testing with id 10
Aa_<-vector()
Aa_[1:100]<-0
Ab_<-vector()
Ab_[1:100]<-0
proa<-0
prob<-0
df$Aa_<-0
df$Ab_<-0
df$propa<-0
df$propb<-0
df$propa2<-0
df$propb2<-0
h<-1
d<-1
listofdf<-list()
for (i in 1:20) {
  listofdf[[i]]<-df[df$id==i,]
}

for (j in 1:20) {
  for (i in 2:100) {
    id<-j
    Aa_[i]<-0
    Ab_[i]<-0
    listofdf[[j]]$propa[1]<-0
    listofdf[[j]]$propb[1]<-0
    listofdf[[j]]$Aa_[1]<-0
    listofdf[[j]]$Ab_[1]<-0
    listofdf[[j]]$Aa_[i]<-listofdf[[j]]$Aa_[i-1]+((1-listofdf[[j]]$choice[i-1])*(listofdf[[j]]$payoff[i-1]))
    listofdf[[j]]$Ab_[i]<-listofdf[[j]]$Ab_[i-1]+((listofdf[[j]]$choice[i-1])*listofdf[[j]]$payoff[i-1])
    listofdf[[j]]$propa[i]<-exp(listofdf[[j]]$Aa_[i])/ (exp(listofdf[[j]]$Aa_[i])+exp(listofdf[[j]]$Ab_[i]))
    listofdf[[j]]$propb[i]<-exp(listofdf[[j]]$Ab_[i])/ (exp(listofdf[[j]]$Aa_[i])+exp(listofdf[[j]]$Ab_[i]))
    listofdf[[j]]$propa2[1]<-0
    listofdf[[j]]$propb2[1]<-0
    Aa_[i]<-Aa_[i-1]+(1-df$choice[df$id==id][i-1])*df$epayoff[df$id==id][i-1]
    Ab_[i]<-Ab_[i-1]+(df$choice[df$id==id][i-1])*df$epayoff[df$id==id][i-1]
    listofdf[[j]]$propa2[i]<-exp(Aa_[i])/ (exp(Aa_[i])+exp(Ab_[i]))
    listofdf[[j]]$propb2[i]<-exp(Ab_[i])/ (exp(Aa_[i])+exp(Ab_[i]))
  }
}

df1<-listofdf[[1]]
for (i in 2:20) {
  x<-listofdf[[i]]
  df1<-rbind(df1,x)
}


L<-vector()
LL<-vector()
parameters<-c(0,0)
ll_rl<- function(parameters){
  parameters[1]<-h
  parameters[2]<-d
  for (i in 1:2000) {
    L[i]<-((h*(1-df1$choice[i])*df1$Aa_)+((df1$choice[i])*df1$Ab_[i]) - (log(exp(h * df1$Aa_[i]) + exp(h * df$Ab_[i]))) )
  }
  LL<-sum(L)
  negLL<- -(LL)
  return(negLL)
}

for (i in 1:2000) {
  L[i]<-(((1-df1$choice[i])*df1$Aa_)+((df$choice[i])*df1$Ab_[i]) - (log(exp(h * df1$Aa_[i]) + exp(h * df$Ab_[i]))) )
}
nlminb(parameters,ll_rl,lower = c(0,0),upper = C(Inf,1))

#bootstrap
df2<-data.frame()
df3<-data.frame()
df4<-data.frame()
for (i in 1:100) {
  ind = sample(1:20, replace = TRUE)
  for (j in ind) {
    x<-which(df1$id==j)
    df4<-df1[x,]
    df2<-rbind(df2,df4)
  }
  df3<- rbind(df2,df3)
  df2<-data.frame()
}

ll_rl<- function(params){
  for (i in 1:2000) {
    L<-h*df$propa[i]+h*df$propb[i]-exp(log(h*Aa_[i]))
    LL<-L+LL
  }
}

params<-c(0,0)
nlminb(params,,ll_rl)
nlminb()
ll_rl <- function(param){ 
  LLv <- c()  
  d<- param[1]
  h<- param[2] 
  for(k in 1:20){
    for(i in 1:100){
      ##creating propensities, which will be used in the likelihood function
      if(i==1){
        dat$propa[i+ 100*(k-1)]<- 0
        dat$propb[i+ 100*(k-1)]<- 0
      }else{
        dat$propa[i+ 100*(k-1)]<- d*dat$propa[i+ 100*(k-1)-1] + ifelse(dat$choice[i+ 100*(k-1)-1]==0,dat$payoff[i+ 100*(k-1)-1],0)
        dat$propb[i+ 100*(k-1)]<- d*dat$propb[i+ 100*(k-1)-1] + ifelse(dat$choice[i+ 100*(k-1)-1]==1,dat$payoff[i+ 100*(k-1)-1],0)
      }
      ##creating LL
      #if chose A
      if(dat$choice[i+ 100*(k-1)]==0){
        LLv[i+ 100*(k-1)] <- h*dat$propa[i+ 100*(k-1)] -log(exp(h*dat$propa[i+ 100*(k-1)])+exp(h*dat$propb[i+ 100*(k-1)]))
        
      }##if chose B
      else{
        
        LLv[i+ 100*(k-1)] <- h*dat$propb[i+ 100*(k-1)] -log(exp(h*dat$propa[i+ 100*(k-1)])+exp(h*dat$propb[i+ 100*(k-1)]))  
        
      } 
    } 
  }
  