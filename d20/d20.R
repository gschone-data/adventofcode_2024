library(data.table)
data<-readLines('/data/schoneg/advent/day20_input.data')
head(data)

for (k in 1:length(data)){
  data[k]=str_replace_all(data[k],pattern="\\#",replacement="1")
  data[k]=str_replace_all(data[k],pattern="\\.",replacement="0")
}



data<-t(as.data.table(sapply(data,str_split,"")))
row.names(data)=NULL

x<-which(data=="E",arr.ind = T)[1]
y<-which(data=="E",arr.ind = T)[2]
end=c(x,y)
x<-which(data=="S",arr.ind = T)[1]
y<-which(data=="S",arr.ind = T)[2]

start=c(x,y)



#grid<-matrix(data),ncol=length(data))
data[start[1],start[2]]
data[end[1],end[2]]
result <- dijkstra(data, start, end)

liste_passage<-which(result!=Inf,arr.ind=T)
result_nb<-nrow(liste_passage)-1
data0<-data
liste_passage0<-liste_passage
as.data.table(liste_passage)->passages

for( k in 1:result_nb){
  x=passages[k,1][[1]]
  y=passages[k,2][[1]]
  data[x,y]="P"
  }
data[start[1],start[2]]="P"
data[end[1],end[2]]="P"
data0<-data
liste_candidats<-matrix(ncol=4,nrow=0)


for(k in 2:(length(data[1,])-1)){
  for(i in 2:(length(data[1,])-1)){
  test1<-paste0(data[k,i-1],data[k,i],data[k,i+1])
  test2<-paste0(data[k-1,i],data[k,i],data[k+1,i])
  if(test1=="P1P"){

    liste_candidats<-rbind(liste_candidats,c(k,i-1,k,i+1))
    }
  if(test2=="P1P"){
    liste_candidats<-rbind(liste_candidats,c(k-1,i,k+1,i))
  }
}
}

###il reste maintenant à trouver l'ordre reel du passage dans liste_passage
next_step<-function(x,y,dt){
  nxt=which((dt[,1]==(x-1)&dt[,2]==y)|
        (dt[,1]==(x+1)&dt[,2]==y)|
          (dt[,1]==x&dt[,2]==(y-1))|
          (dt[,1]==x&dt[,2]==(y+1)))
return(nxt)

}

ordre<-matrix(nrow=0,ncol=2)
pass0=which(passages[,1]==start[1]&passages[,2]==start[2])
passages2<-passages[-pass0]
ordre<-rbind(ordre,start)
while(nrow(passages2)!=0){
  x=ordre[nrow(ordre),1][[1]]
  y=ordre[nrow(ordre),2][[1]]
  tmp<-next_step(x,y,passages2)
  ordre<-rbind(ordre,passages2[tmp,],use.names=F)
  passages2<-passages2[-tmp]




}

fin<-matrix(ncol=3,nrow=0)
for(c in 1:nrow(liste_candidats)){
    cheat_un<-liste_candidats[c,1:2]
    cheat_deux<-liste_candidats[c,3:4]
    place_un<-which(ordre$V1==cheat_un[[1]]&ordre$V2==cheat_un[[2]])
    place_deux<-which(ordre$V1==cheat_deux[[1]]&ordre$V2==cheat_deux[[2]])
    gain=abs(place_deux-place_un)-2
    cheat<-c(gain,(liste_candidats[c,1]+liste_candidats[c,3])/2,(liste_candidats[c,2]+liste_candidats[c,4])/2)

    if (gain>=100){fin<-rbind(fin,cheat)}

}

fin_dt<-as.data.frame(fin)
fin_dt |> group_by(V1) |> summarise(n())
