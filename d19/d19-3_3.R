library(data.table)
library(stringr)
data<-data.table(readLines("/data/schoneg/advent/input19.data"))
cible<-data[3:nrow(data),]
input<-data.table(unlist(str_split(data[1,],",")))

input$V1=trimws(input$V1)


res<-vector()
co<-vector()
tt<-data.table(matrix(ncol=2,nrow=0))
colnames(tt)<-c("txt","ct")
fin<-tt
for (c in 1:nrow(cible)){
  tt<-rbind(tt,data.table(cible$V1[c],1),use.names=F)
  boucle<-T

  while(boucle==T&nrow(tt!=0)){
    tn=NULL
    ct=NULL
    for(r in 1:nrow(tt)){
      t<-tt$txt[r]
      count<-tt$ct[r]
      liste_str<-sapply(1:min(8,nchar(t)),function(x) {substr(t,nchar(t)-x+1,nchar(t))})
      test<-which(sapply(liste_str,function(x) {nrow(input[V1==x,])>0}))
      if(length(test)!=0){
        tn<-c(tn,sapply(test,function(x) {substr(t,1,nchar(t)-x)}))
        ct<-c(ct,as.numeric(rep(count,length(test))))
        }

    }
    tt<-data.table(matrix(ncol=2,nrow=0))
    colnames(tt)<-c("txt","ct")
    if(length(tn)==0){boucle<-F;res[c]=F;break}else{

      tt<-rbind(tt,data.table(cbind(tn,ct)),use.names=F)
      tt<-  tt |> group_by(txt) |> summarise(ct=sum(as.numeric(ct))) |> setDT()
      fin<-rbind(fin,tt[txt=="",])
      tt<-  tt[txt!="",]

    }
  }
  co[c]=sum(fin$ct)
  res<-co[c]>0
  fin<- data.table(matrix(ncol=2,nrow=0))
  colnames(fin)<-c("txt","ct")
  }
sum(co)
