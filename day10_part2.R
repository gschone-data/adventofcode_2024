options(sc=999)
# library -----------------------------------------------------------------
library(data.table)
library(stringr)
library(dplyr)



# data --------------------------------------------------------------------
data<-read.delim("input10.data",sep="",header=F,colClasses = "character")
data$V1=as.character(data$V1)
source("function_day10.R")


# repérer les 9 (part2) -----------------------------------------------------------
liste9<-data.table(matrix(nrow=0,ncol=2))
colnames(liste9)=c("x","y")

for (n in 1:nrow(data)){
 tmp<-str_locate_all(data[n,],"9")

liste9<-rbind(liste9,(data.table(x=n,y=(tmp[[1]][,1]))))

}

# repérer les suites dans un sens prendre le max consécutif 
#n+1 à n+?
#chemins
result=0
for(i in 1:(nrow(liste9))){
  max=9
  res_k<-liste9[i,]
  res_k$k=9
  res_k$n=1
  tmp_k<-data.table(matrix(nrow=0,ncol=4))
  colnames(tmp_k)=c("x","y","k","n")
  k=8
    while (k>=0&nrow(res_k)!=0){
       nb_k1<-nrow(res_k)
      
      for (j in 1:nb_k1){
      tmp_k=rbind(tmp_k,next_k(res_k[j]$x,res_k[j]$y,k,res_k[j]$n))
      }
      k=k-1
      max=max-1
      tmp_k[,nb_chem:=.N,by=c("x","y")]
      tmp_k[,n:=nb_chem*n]
      tmp_k$nb_chem=NULL
      res_k<-tmp_k[!duplicated(paste0(x,"-",y))]
      tmp_k<-data.table(matrix(nrow=0,ncol=4))
      colnames(tmp_k)=c("x","y","k","n")
    }
  if(nrow(res_k)!=0){
    #res_k |> group_by(x,y) |> summarise(tot=n())->tmp
    #print(tmp)
#    print(res_k)
    result=result+sum(res_k$n)
 #   print(result)
    #print(liste0[i])
    
  }
}
liste0<-data.table(matrix(nrow=0,ncol=2))
colnames(liste0)=c("x","y")

for (n in 1:nrow(data)){
  tmp<-str_locate_all(data[n,],"0")
  
  liste0<-rbind(liste0,(data.table(x=n,y=(tmp[[1]][,1]))))
  
}


result=0
for(i in 1:(nrow(liste0))){
  res_k<-liste0[i,]
  res_k$k=0
  res_k$n=1
  tmp_k<-data.table(matrix(nrow=0,ncol=4))
  colnames(tmp_k)=c("x","y","k","n")
  k=1
  while (k<10&nrow(res_k)!=0){
    nb_k1<-nrow(res_k)

    for (j in 1:nb_k1){
      tmp_k=rbind(tmp_k,next_k(res_k[j]$x,res_k[j]$y,k,res_k[j]$n))
    }
    k=k+1
    tmp_k[,n:=sum(n),by=c("x","y")]
    #tmp_k[,n:=nb_chem*n]
    #tmp_k$nb_chem=NULL
    res_k<-tmp_k[!duplicated(paste0(x,"-",y))]
    tmp_k<-data.table(matrix(nrow=0,ncol=4))
    colnames(tmp_k)=c("x","y","k","n")

      }
  if(nrow(res_k)!=0){
    #res_k |> group_by(x,y) |> summarise(tot=n())->tmp
    #print(tmp)
   # print(res_k)
    result=result+sum(res_k$n)
    print(result)
    #print(liste0[i])
    
  }
}

print(result)
