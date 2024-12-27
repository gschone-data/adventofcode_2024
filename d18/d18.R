library(data.table)
library(stringr)
library(dplyr)
source("d18/f18.R")

data<-data.table(readLines("inputs/d18.data"))
xlim<-ylim<-70
#xlim<-ylim<-6


data$y<-as.integer(str_extract(data$V1,"\\d+(?=,)"))
data$x<-as.integer(str_extract(data$V1,"(?<=,)\\d+"))
data$V1=NULL

p0<-list(x=0,y=0)
end<-list(x=xlim,y=ylim)

#1024 premiers éléments
obs<-data[1:1024]


pos<-p0
dist<-calc_dist(p0,end)
tmp<-data.table(matrix(nrow=0,ncol=4))
colnames(tmp)=c("x","y","d","n")
pos$d=dist
pos$n=1
tmp<-rbind(tmp,pos)
res<-tmp
prev<-tmp
while(nrow(prev)!=0){
  for(p in 1:nrow(prev)){
    pos=prev[p,]
    
    
    opt<-lister_options(pos)
    for (o in 1:nrow(opt)){
      op<-opt[o,]
      nxt<-calc_options(pos,op)
      d<-calc_dist(nxt,end)
      n<-pos$n+1
      tst<-res[x==nxt$x&y==nxt$y]
      if(nrow(tst)==0){
        tmp<-rbind(tmp,data.table(x=nxt$x,y=nxt$y,d=d,n=n))
      }else{
        if(tst$n>n){
          tmp<-rbind(tmp,data.table(x=nxt$x,y=nxt$y,d=d,n=n))
        }
      }
    }
  }
  setorder(tmp,n)
  tmp<-tmp[!duplicated(tmp[,.(x,y)])]
  res<-rbind(res,tmp)
  setorder(res,n)
  res<-res[!duplicated(res[,.(x,y)])]
  prev<-tmp
  if(nrow(tmp)!=0){tmp<-tmp[-(1:nrow(tmp))]}
  
  
}  

saveRDS(res,"d18res.data")


size <- 71

grid <- create_grid(size,  obs)
start <- c(0, 0)
end <- c(70, 70)
result <- dijkstra(grid, start, end)

# Résultat
print(result$distances[end[1] + 1, end[2] + 1])  # Distance minimale
print(result$path)  # Chemin


