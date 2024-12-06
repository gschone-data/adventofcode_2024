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
position_next_plus<-function(p){
  x=as.integer(p[1])
  y=as.integer(p[2])
  direction=p[3]
  if(test_position(position_next(p),data)=="-1"){change_dir(p)}
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



p=c(40,47,"N")
procedure<-function(p){

positions_new=data.frame()
positions_new=rbind(positions_new,p)
colnames(positions_new)=c("x","y","dir")
count=1
p1=p
repeat{
  test=test_position(position_next(p1),data) 
  
  if(test==-1){p1=change_dir(p1)}
  if(test==0){
    nb_fin<<-nb_fin+1
    break}

  if(nrow(positions_debut |> filter(x==p1[1]&y==p1[2]))==0){
    nb_newpos<<-nb_newpos+1
    break
  }
  p1=position_next(p1)
  if(positions_debut[x==p1[1]&y==p1[2]&dir==p1[3],.N]>0){
    nb_loop<<-nb_loop+1
    print(k)
    break
  }
  positions_new<-rbind(positions_new,p1)
  count=count+1

}
}
nb_fin=0
nb_newpos=0
nb_loop=0
list_dir<-c("N","E","O","S")
for (k in 2:4724){
  print(k)
  positions_debut<-setDT(positions[1:k,])
  p=position_next(change_dir(as.character(positions_debut[k,])))
    
  procedure(p)
  }
pos_obst=data.frame()
nb_fin=0
nb_newpos=0
nb_loop=0
for (k in 2:4724){
  if(test_position(position_next(positions[k]))!=-1){
  positions_debut<-setDT(positions[1:k,])
  
  p=position_next_plus(as.character(positions_debut[k,]))
  
  if(positions_debut[x==p[1]&y==p[2]&dir==p[3],.N]>0){
    nb_loop=nb_loop+1
    pos_obst=rbind(pos_obst,position_next(as.character(positions_debut[k,])))
    print(k)}
}
}
