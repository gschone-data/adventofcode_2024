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
p0=c(40,47,"N")
    
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

  if(test_position(position_next(p),data2)==-1){p=change_dir(p)}
  result=case_when(p[3]=="N"~c(as.integer(p[1])-1,as.integer(p[2]),"N"),
                   p[3]=="O"~c(as.integer(p[1]),as.integer(p[2])-1,"O"),
                   p[3]=="S"~c(as.integer(p[1])+1,as.integer(p[2]),"S"),
                   p[3]=="E"~c(as.integer(p[1]),as.integer(p[2])+1,"E"))
  return(result)
}  

proj<-function(){

  if(test_position(position_next(p),data2)==-1){p=change_dir(p)}
  result=case_when(p[3]=="N"~c(as.integer(p[1])-1,as.integer(p[2]),"N"),
                   p[3]=="O"~c(as.integer(p[1]),as.integer(p[2])-1,"O"),
                   p[3]=="S"~c(as.integer(p[1])+1,as.integer(p[2]),"S"),
                   p[3]=="E"~c(as.integer(p[1]),as.integer(p[2])+1,"E"))
  p<<-result

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




pos_obst=data.table(matrix(nrow=0,ncol=3))
colnames(pos_obst)=c("x","y","dir")
nb_loop=0
for (k in 1:(nrow(positions)-1)){
  print(k)
  data2<-data
  obst<-positions[k+1,]
  substr(data2$V1[as.integer(obst[1])],as.integer(obst[2]),as.integer(obst[2]))="#"
  p<-as.character(positions[k,])
  temp<-replicate(130,proj())
  if(test_out(p)==F){
    temp<-replicate(1000,proj())
    if(test_out(p)==F){
      temp<-replicate(3900,proj())
      if(test_out(p)==F){
        nb_loop=nb_loop+1
        print(nb_loop)
        pos_obst=rbind(pos_obst,as.list(obst))
      }
    }
  }
    
}
  
test_out<-function(p){
  return(as.integer(p[1])>130|as.integer(p[1])<1|as.integer(p[2])<1|as.integer(p[2])>130)
  
}
p=p0
temp<-replicate(10,proj())
  
pos_obst |> group_by(x,y) |> summarise(tot=n())->pos_obst_tot




vec=vector()
colnames(pos_out)=c("x","y","dir")
nb_out=0
for (k in 1:(nrow(positions)-1)){
  data2<-data
  obst<-positions[k+1,]
  substr(data2$V1[as.integer(obst[1])],as.integer(obst[2]),as.integer(obst[2]))="#"
  p<-as.character(positions[k,])
  temp<-replicate(130,proj())
  if(test_out(p)==T){
  nb_out=nb_out+1
  vec[nb_out]=k
  }
}
    temp<-replicate(1000,proj())
    if(test_out(p)==F){
      temp<-replicate(3900,proj())
      if(test_out(p)==F){
        nb_loop=nb_loop+1
        print(nb_loop)
        pos_obst=rbind(pos_obst,as.list(obst))
      }
    }
  }
  
}
