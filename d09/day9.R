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
#saveRDS(res_liste,"day9.datamodif.data")
res<-as.vector(readRDS("day9.datamodif.data"))
# Sys.time()
#   x<-which(res==".")[1]
#   while(!is.na(x)){
#     res[x]=res[length(res)]
#     res<-res[-length(res)]
#    # while(res[length(res),1]=="."){
#      # res=res[1:(length(res)-1)]
#       
#   #  }
#     x<-which(res==".")[1]
#     }
#   Sys.time()
#   saveRDS(res,"res9.rds")
# res_index<-seq(0,length(res)-1)
# res_liste=as.numeric(res)
# res_index=as.vector(res_index)
# res<-as.data.frame(cbind(res_index,res_liste))
# 
# res$prod=res$res_index*res$res_liste
# x<-sum(res$prod)

# part 2 ------------------------------------------------------------------
prep_str<-list()
for (i in 1:nrow(split)){
  
  if (i%%2!=0){
    #impair -> Data
    prep_str=append(prep_str,
                     rep("-",as.integer(split[i,])))
  }else{
    #pair
    prep_str=append(prep_str,
                     rep(".",as.integer(split[i,])))
  }
}
string_list<-paste0(prep_str,collapse="")
saveRDS(string_list,"day92.stringlist.data")


string_list<-readRDS("day92.stringlist.data")
res<-as.vector(readRDS("day9.datamodif.data"))
res_liste<-res 
# 
# string_list<-paste0(res_liste,collapse="")
# string_list<-string_liste0
# res_liste=res_liste0


res_liste<-as.numeric(res_liste)
maxID<-max(res_liste,na.rm=T)

Sys.time()
k=maxID
while(k>=1){
  pos_move<-which(res_liste==k)
  taille_move<-length(pos_move)
  places_vide<-str_locate(string_list,paste0(rep("\\.",taille_move),collapse=""))
  if (!is.na(places_vide[1])){
    if (places_vide[[1]]<pos_move[1]){
  #on sait qu'il y a une place pour le bloc
    place_move<-pos_move[1]
    for (j in 0:(taille_move-1)){
    res_liste[places_vide[[1]]+j]=k
    res_liste[place_move+j]=NA
    substr(string_list,places_vide[[1]]+j,places_vide[[1]]+j)="-"
    substr(string_list,place_move+j,place_move+j)="."
    }
    
    while(is.na(tail(res_liste,1))){
      res_liste<-res_liste[-length(res_liste)]
      substr(string_list,nchar(string_list),nchar(string_list))=""
    }
    }  }  
  k<-k-1
  }

res_index<-as.vector(seq(0,length(res_liste)-1))
res_liste<-as.vector(res_liste)
res<-as.data.frame(cbind(res_index,res_liste))
res$prod=res$res_index*res$res_liste
x<-sum(res$prod,na.rm=T)
