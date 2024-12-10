library(stringr)
library(data.table)
data<-readLines("input9.data")
test1<-"2333133121414131402"
data<-test1
split<-as.data.frame(str_split(data,""))
colnames(split)="car"

res_liste<-list()
count_pair = 0
for (i in 1:nrow(split)){
  
  if (i%%2!=0){
    #impair -> Data
    res_liste=append(res_liste,
                       rep(count_pair,as.integer(split[i,])))
    count_pair=count_pair+1
  }else{
    #pair
    res_liste=append(res_liste,
                     rep(".",as.integer(split[i,])))
  }
}
saveRDS(res_liste,"day9.data.modif")

  res<-as.vector(readRDS("day9.data.modif"))
  Sys.time()
  x<-which(res==".")[1]
  while(!is.na(x)){
    res[x]=res[length(res)]
    res<-res[-length(res)]
   # while(res[length(res),1]=="."){
     # res=res[1:(length(res)-1)]
      
  #  }
    x<-which(res==".")[1]
    }
  Sys.time()
  saveRDS(res,"res9.rds")
res_index<-seq(0,length(res)-1)
res_liste=as.numeric(res)
res_index=as.vector(res_index)
res<-as.data.frame(cbind(res_index,res_liste))

res$prod=res$res_index*res$res_liste
x<-sum(res$prod)
