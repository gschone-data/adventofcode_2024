library(stringr)
library(data.table)
library(dplyr) 
# data_string<-"day14_test.data"
# lim_x=11
# lim_y=7

 data_string<-"input14.data"
lim_x=101
 lim_y=103

data_str<-readLines(data_string)
data<-str_split(data_str," ")
data<-transpose(setDT(data))
  
data$V1=substr(data$V1,3,nchar(data$V1))  
data$V2=substr(data$V2,3,nchar(data$V2))
data$x=substr(data$V1,1,str_locate(data$V1,",")[1]-1)
data$x=str_extract(data$V1,".*(?=,)")
data$y=str_extract(data$V1,"(?<=,).*")

data$vx=str_extract(data$V2,".*(?=,)")
data$vy=str_extract(data$V2,"(?<=,).*")
data$V1=NULL
data$V2=NULL

data |> mutate(vx=as.numeric(vx),
               vy=as.numeric(vy),
               x=as.numeric(x),
               y=as.numeric(y))->data


next_pos_x<-function(x,vx){
  x=(x+100*vx)%%lim_x
return(x)
 }
next_pos_y<-function(y,vy){
y=(y+100*vy)%%lim_y
return(y)
}

data$finx=next_pos_x(data$x,data$vx)
data$finy=next_pos_y(data$y,data$vy)

quartier<-function(x,y){
  midx=(lim_x-1)/2
  midy=(lim_y-1)/2
  res=case_when(
    (x == midx|y == midy)~0,
    (x < midx & y < midy)~1,
    (x < midx & y > midy)~2,
    (x > midx & y < midy)~3,
    (x > midx & y > midy)~4,
    T~5)
  return(res)
}


data$quartier=quartier(data$finx,data$finy)
data |> group_by(quartier) |> summarise(tot=n())->fin
library(ggplot2)
ggplot(data)+
  geom_point(aes(x=finx,y=finy))+
  scale_y_reverse(limits=c(lim_y, 0))

fin |> filter(quartier!=0) |> summarise(prod(tot))

