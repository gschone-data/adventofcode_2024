library(data.table)
library(dplyr)
library(tidyverse)
data <-read.delim("input4.data",sep="",header=F)
setDT(data)

comptage_xmas<-function(row){
  
  sens1<-str_count(row,"XMAS")
  sens2<-str_count(row,"SAMX")
  tot=sens1+sens2
  return(tot)
}
#matrice transpose
for (k in 1:nrow(data)){
  temp=as.data.frame(str_split(data[k,],""),col.names = k)
  if (k==1){data_vert<-temp}else{data_vert=cbind(data_vert,temp)}  
  
}

data_hori<-data.table::transpose(data_vert)
data_vert <-data_vert |> unite(col="res",sep="")
row=as.character(data[1,])

sens_normal=apply(data,1,comptage_xmas)
sens_vertical=apply(data_vert,1,comptage_xmas)


sum(sens_normal)
sum(sens_vertical)
row2<-data_hori[1,]


count_diag<-function(df,ligne){
  res=0
  x_pos=which(df[ligne,]=="X")
  for (pos in x_pos){
  if (ligne-3>0){
    #on cherche les diag superieures
  if((pos-3)>0){
    if (df[ligne-1,pos-1]=="M"&df[ligne-2,pos-2]=="A"&df[ligne-3,pos-3]=="S"){
      res=res+1}
    }
  if((pos+3)<=ncol(df)){
    if(df[ligne-1,pos+1]=="M"&df[ligne-2,pos+2]=="A"&df[ligne-3,pos+3]=="S"){
      res=res+1
    }
  }
  }
  if (ligne+3<=nrow(df)){
    if((pos-3)>0){
      if (df[ligne+1,pos-1]=="M"&df[ligne+2,pos-2]=="A"&df[ligne+3,pos-3]=="S"){
        res=res+1}
    }
    if((pos+3)<=ncol(df)){
      if(df[ligne+1,pos+1]=="M"&df[ligne+2,pos+2]=="A"&df[ligne+3,pos+3]=="S"){
        res=res+1
      }
    }
  }
    
    
  }
  return(res)
}
diag=0
vec=vector()
for (i in 1:140){

vec[i]=count_diag(data_hori,i)

diag=diag+vec[i]
}
count_horizontal<-function(df,ligne){
  res=0
  x_pos=which(df[ligne,]=="X")
  for (pos in x_pos){
    if ((pos-3)>0){
      if(df[ligne,pos-1]=="M" & df[ligne,pos-2]=="A" & df[ligne,pos-3]=="S"){
        res=res+1
        }
    }
    if ((pos+3)<=ncol(df)){
      if(df[ligne,pos+1]=="M"&df[ligne,pos+2]=="A"&df[ligne,pos+3]=="S"){
        res=res+1
        }
          }
      
    }
    
return(res)    
  }
  
  


final = diag + sum(sens_normal)+sum(sens_vertical)

