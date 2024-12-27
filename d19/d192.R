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


for (c in 1:nrow(input)){
  t<-input$V1[c]
  boucle=T
  input2<-input[-c]
  while(t!=""&boucle==T){
    for (i in min(max_car,nchar(t)):1){
      boucle=F
      test<-substr(t,nchar(t)-i,nchar(t))
      tmp<-input2[V1==test,]
      if(nrow(tmp)>0){t<-substr(t,i+1,nchar(t));boucle=T;break}
    }

  }

   res[c]=t
}

input_n<-as.data.table(cbind(input$V1,res))
input_n[trimws(res)!=""]->input
input$res=NULL
input$nchar=nchar(input$V1)

setorder(input,-nchar)
max_car<-max(input$nchar)
res<-vector()


for (c in 1:nrow(cible)){
  t<-cible$V1[c]
  boucle=T
  while(t!=""&boucle==T){
    for (i in min(max_car,nchar(t)):1){
      boucle=F
      test<-substr(t,nchar(t)-i,nchar(t))
      tmp<-input[V1==test,]
      if(nrow(tmp)>0){t<-substr(t,1,nchar(t)-i);boucle=T;break}


    }

  }
  res[c]=t
}

cible_fin<-data.table(cbind(cible$V1,res))
cible_fin[res=="",.N]
cible_2<-cible_fin[res!=""]

res<-vector()


for (c in 1:nrow(cible_2)){
  t<-cible_2$V1[c]
  boucle=T
  occ=T
  while(t!=""&boucle==T){
    for (i in min(max_car,nchar(t)):1){
      boucle=F
      test<-substr(t,nchar(t)-i,nchar(t))
      tmp<-input[V1==test,]
      if(nrow(tmp)>0&occ==T){t<-substr(t,1,nchar(t)-i);boucle=T;break}
      if(nrow(tmp)>0&occ==F){occ=T}

    }

  }
  res[c]=t
}
cible_fin2<-data.table(cbind(cible_2$V1,res))
cible_fin2[res=="",.N]
cible_2<-cible_fin[res!=""]

