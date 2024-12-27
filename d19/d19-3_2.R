library(data.table)
library(stringr)
data<-data.table(readLines("/data/schoneg/advent/input19.data"))
cible<-data[3:nrow(data),]
input<-data.table(unlist(str_split(data[1,],",")))

input$V1=trimws(input$V1)
input$nchar=nchar(input$V1)
setorder(input,-nchar)
max_car<-max(input$nchar)
res<-vector()
co<-vector()


for (c in 1:nrow(cible)){
  tt<-cible$V1[c]
  boucle<-T
  count<-0
  while(boucle<-T){
    tn=NULL
    for(t in tt){
    liste_str<-sapply(1:min(8,nchar(t)),function(x) {substr(t,nchar(t)-x+1,nchar(t))})
    test<-which(sapply(liste_str,function(x) {nrow(input[V1==x,])>0}))
    tt<-tt[tt!=t]
    if(length(test)!=0){
      tn<-c(tn,sapply(test,function(x) {substr(t,1,nchar(t)-x)}))
    }

    }
    if(length(tn)==0){boucle<-F;res[c]=F;break}else{
      count<-count+sum(tn=="")
      if(count>0){break}
      #dt_tn2[,count:=.N,tn]
      tt<-tn
      print(tt)
        }
    #if(sum(tn=="")>0){res[c]=T;count<-count+1}
  }
  co[c]=count
  res[c]=count>0
}
sum(co)
