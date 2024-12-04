library(data.table)
library(dplyr)
data <-read.delim("input2.data",sep="",header=F)
setDT(data)
data$nb_lvl=8-rowSums(is.na(data))

test_croissance<-function(row){
  lvl = row[9]
  temp=""
  for (i in 1:(lvl-1)){
    temp<-case_when(row[i]<row[i+1] ~ paste0(temp,"A"),
                    row[i]>row[i+1]~  paste0(temp,"D"),
                    T~"E")
  } 
  
    result=(temp==paste0(replicate(lvl-1,"A"),collapse = "") | temp==paste0(replicate(lvl-1,"D"),collapse = ""))
   return(result)
}

test_ecart<-function(row){
  lvl = row[9]
  temp=T
  for (i in 1:(lvl-1)){
    temp<-temp*(abs(row[i+1]-row[i])<4)
               
  } 
   return(temp) 
  
}

data$test_croissance=apply(data,1,test_croissance)
data$test_ecart=apply(data,1,test_ecart)
data[,.N,test_ecart*test_croissance]
