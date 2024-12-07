library(data.table)
library(stringr)
library(dplyr)

#data<-readLines("input61.data")
#data<-as.data.frame(data)#data <-read.delim("input6_11.data",sep="\n",header=F)
data <-read.delim("input6_1.data",sep="",header=F)
setDT(data)
data$index=seq.int(nrow(data))
data$v2=str_detect(data$V1,"\\^")
i=data[v2==T,index]
j=unlist(gregexpr('\\^', data[i,]))
dir="N"
x=40
y=47
direction="N"
#position_0
p=c(40,47,"N")

position_next<-function(p){
  x=as.integer(p[1])
  y=as.integer(p[2])
  direction=p[3]
  result=case_when(direction=="N"~c(x-1,y,"N"),
                   direction=="O"~c(x,y-1,"O"),
                   direction=="S"~c(x+1,y,"S"),
                   direction=="E"~c(x,y+1,"E"))
  return(result)
}  
test_position<-function(p,data){
  x=as.integer(p[1])
  y=as.integer(p[2])
  if ((x>nrow(data) | x<1)|(y>nchar(data$V1[1]) | y<1))
  {res=0}else
  {if(data[x,substr(V1,y,y)]=="#"){
    res=-1}else
    {res=1}}
  return(res)
}
change_dir<-function(p){
  x=as.integer(p[1])
  y=as.integer(p[2])
  direction=p[3]
  direction=case_when(direction=="N"~"E",
                      direction=="E"~"S",
                      direction=="S"~"O",
                      direction=="O"~"N")
  
  
  return(c(x,y,direction))
}


positions=data.frame()
  
  p=c(40,47,"N")
  positions=rbind(positions,p)
  colnames(positions)=c("x","y","dir")
  count=1
  p1=p
  repeat{
    test=test_position(position_next(p1),data)
    if(test==-1){p1=change_dir(p1)}
    if(test==0)
    {break
    }
    p1=position_next(p1)
    positions<-rbind(positions,p1)
    count=count+1
    
  }

positions |> group_by(x,y) |> summarise(nb_pass=n())->total
total
total$nb_pass>1
setDT(total )
saveRDS(positions,"positions.data")
