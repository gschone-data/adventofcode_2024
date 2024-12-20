library(stringr)
library(data.table)
library(dplyr)

source("day15_f.R")
data_str<-readLines("day15.data")
data<-str_split(data_str," ")
data<-data.table(data)
separation<-which(data$data=="")

data[1:(separation-1),]->puzzle

puzzle<-str_split(puzzle$data,"")
puzzle<-as.data.frame(puzzle)
puzzle<-transpose(setDT(puzzle))
puzzle_origin<-puzzle

data[-(1:(separation)),]->moves
moves<-unlist(sapply(moves$data,str_split,""))


p0<-get_start(puzzle)
pos<-p0
for (k in 1:length(moves)){
  sens<-moves[k]
  pos<-deplacer_curs(pos,sens)
}


res<-data.table(matrix(ncol=2,nrow=0))
colnames(res)=c("x","y")
for(r in 1:nrow(puzzle)){
tmp=data.table(x=(r-1)*100,y=which(puzzle[r,]=="O")-1)
res<-rbind(res,tmp) 
}
res$tot=res$x+res$y
sum(res$tot)
