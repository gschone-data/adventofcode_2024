library(data.table)

data <-read.delim("input4.data",sep="",header=F)
setDT(data)
for (k in 1:nrow(data)){
  temp=as.data.frame(str_split(data[k,],""),col.names = k)
  if (k==1){data_vert<-temp}else{data_vert=cbind(data_vert,temp)}  
  
}
data_hori<-data.table::transpose(data_vert)


count_xmas<-function(df,ligne){
  res=0
  A_pos=which(df[ligne,]=="A")
  for (pos in A_pos){
  if ((ligne-1)>0 & (ligne+1)<=nrow(df)& (pos-1)>0 & (pos+1)<=ncol(df)){
    val_1<-df[ligne-1,pos-1]
    val_2<-df[ligne-1,pos+1]
    val_3<-df[ligne+1,pos-1]
    val_4<-df[ligne+1,pos+1]
  if (str_count(paste0(c(val_1,val_2,val_3,val_4),collapse = ""),"S")==2 &
      str_count(paste0(c(val_1,val_2,val_3,val_4),collapse = ""),"M")==2 & 
      val_1!=val_4){
    res=res+1
  }
  }
  }
  return(res)
}
echantillon<-data_hori[1:20,1:20]
vec=vector()
result=0
for (i in 2:139){
  temp<-count_xmas(data_hori,i)
vec[i]=temp
  result=result+temp
}
sum(vec,na.rm = T)
max(vec,na.rm=T) 

x=count_xmas(data_hori,6)
