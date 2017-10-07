#Dynamic Programming

load("~/Desktop/R Files/R Directory/graph.RData")
df<-graph

jnew=integer(0)
jnew[1:6]=Inf
jnew[7]<-0
names(jnew)=graph$node

j=integer(length(jnew))
names(j)=graph$node


while(!all(jnew==j)){
  j=jnew
  for (i in 1:length(j)) {
    c=graph$distance[[i]]
    jnew[i]=min(c+j[graph$adj[[i]]])
  }
}
