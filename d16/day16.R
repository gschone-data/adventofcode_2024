##on va partir de la fin "E" 
##on va explorer tous les chemins possible jusqu'à S 
library(data.table)
library(stringr)
source("day16_f.R")
data_str<-readLines("day16.data")
data<-str_split(data_str," ")
data<-data.table(data)

data<-str_split(data$data,"")
data<-as.data.frame(data)
data<-transpose(setDT(data))

start=get_car(data,"S")
end=get_car(data,"E")

pos<-start
dist0<-calc_dist(start,end)
tmp<-data.table(matrix(nrow=0,ncol=5))
colnames(tmp)=c("x","y","d","dist","c","tot")
res<-tmp
pos$dist=dist0
pos$c=0
pos$tot=1
res<-rbind(res,pos)
liste_opt<-data.table(matrix(ncol=3,nrow=0))
colnames(liste_opt)=c("dx","dy","d")
prev<-res
while(nrow(prev)!=0){
  for(l in 1:nrow(prev)){
   opt<-lister_options(prev[l,])
   nb_opt=nrow(opt)
   if(nb_opt!=0){
   for (i in 1:nrow(opt)){
     o<-opt[i,]
     nxt<-calc_options(prev[l,],o) 
     dist<-calc_dist(nxt,end)
     c<-prev[l,c]+ifelse(o$dx!=0|o$dy!=0,1,0)+ifelse(nxt$d==prev[l,d],0,1000)
     tst<-res[x==nxt$x&y==nxt$y]
     if(nrow(tst)==0){
     tmp<-rbind(tmp,data.table(x=nxt$x,y=nxt$y,d=nxt$d,dist=dist,c=c))
     }else{
       if(tst$c>c){
         tmp<-rbind(tmp,data.table(x=nxt$x,y=nxt$y,d=nxt$d,dist=dist,c=c))
         
       }
     }
       
     
     }
   
   }
  }
  setorder(tmp,c)
  tmp<-tmp[!duplicated(tmp[,.(x,y)])]
  #tmp<-anti_join(tmp,res,by=join_by("x","y"))
  res<-rbind(res,tmp)
  setorder(res,c)
  res<-res[!duplicated(res[,.(x,y)])]
  prev<-tmp
  if(nrow(tmp)!=0){tmp<-tmp[-(1:nrow(tmp))]}
  }
Sys.time()
