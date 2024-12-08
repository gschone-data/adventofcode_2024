library(data.table)
library(dplyr)
data <-read.delim("input2.data",sep="",header=F)
setDT(data)
#data$nb_lvl=8-rowSums(is.na(data))

test_croissance<-function(row,n_col){
  lvl = n_col- sum(is.na(row))
  temp=""
  for (i in 1:(lvl-1)){
    temp<-case_when(row[i]<row[i+1] ~ paste0(temp,"A"),
                    row[i]>row[i+1]~  paste0(temp,"D"),
                    T~"E")
  } 
  
  result=(temp==paste0(replicate(lvl-1,"A"),collapse = "") | temp==paste0(replicate(lvl-1,"D"),collapse = ""))
  return(result)
}

test_ecart<-function(row,n_col){
  lvl = n_col-sum(is.na(row))
  temp=T
  for (i in 1:(lvl-1)){
    temp<-temp*(abs(row[i+1]-row[i])<4)
    
  } 
  return(temp) 
  
}

data0$test_croissance=apply(data,1,test_croissance,8)
data0$test_ecart=apply(data,1,test_ecart,8)
data0$index<-seq.int(nrow(data0))
data0=data0[test_ecart*test_croissance==1]

for(k in  1:8){
  data_temp<-data
  data_temp[,k]=NULL
  data_temp$test_croissance=apply(data_temp,1,test_croissance,7)
  data_temp$test_ecart=apply(data_temp,1,test_ecart,7)
  data_temp$index<-seq.int(nrow(data_temp))
  data_temp<-data_temp[test_ecart*test_croissance==1]
  assign(paste0("data",k),data_temp)
}
c(data1$index,
      data2$index,
      data3$index,
      data4$index,
      data5$index,
      data6$index,
      data7$index,
      data8$index)->liste_ok

length(unique(liste_ok))
      