library(stringr)
library(data.table)
library(glue)
source("function_day12.R")
library(dplyr)

data<-read_data("input12.data")
#data<-read_data("test_day12_2.data")
type_farms<-data[, unique(unlist(lapply(.SD, unique)))]

tmp<-data.table(matrix(ncol=3,nrow=0))
colnames(tmp)=c("x","y","t")
for (k in type_farms){
  for (i in 1:nrow(data)){
    tmp<-rbind(tmp,data.table(x=i,y=which(data[i,]==k),t=k))
  }
}

dt_farms<-tmp



dt_f<-dt_farms
dt_f$s=dt_f$x+dt_f$y
setorder(dt_f,t,s)
fin<-data.table(matrix(ncol=5,nrow=0))
for (a in type_farms){
  nb_f=0
  dt_a<-dt_f[t==a]
  dt_a[1,f:=0]
  a_repartir<-dt_a[-1,]
  a_result<-dt_a[1,]
  tmp_res<-a_result

  while(nrow(a_repartir)>0){
      test<-data.table(matrix(ncol=5,nrow=0))
      for(i in 1:nrow(a_result)){
        tmp <- a_result[i,]
        f_<-tmp$f
        a_repartir$dist<-dist_(a_repartir$x,a_repartir$y,tmp$x,tmp$y)
        tmp_res<-a_repartir[dist==1,]
        tmp_res$dist=NULL
        if (nrow(tmp_res)>0){
        a_repartir<-a_repartir |> anti_join(tmp_res,by = join_by(x, y, t, s, f))
        tmp_res[,f:=f_]
        a_result<-rbind(a_result,tmp_res)  
        test<-rbind(tmp_res,test,use.names=F)
        }
      }
      if(nrow(test)==0){
        nb_f=nb_f+1
        tmp_res<-a_repartir[1,]
        tmp_res$f=nb_f
        tmp_res$dist=NULL
        a_result<-rbind(a_result,tmp_res)
        a_repartir<-a_repartir[-1,]
         
      }

  }
  fin<-rbind(a_result,fin,use.names=F)
  
}
saveRDS(fin,"res_prov_12.rds")
fin <-as.data.frame(fin)
for (i in 1:nrow(fin)){
  tmp<-4-nb_commun(fin[i,])
  fin[i,]$perim<-tmp
  
}

fin |> group_by(t,f) |> summarise(perim=sum(perim),aire=n())->fin_p
fin_p$result=fin_p$perim*fin_p$aire  
sum(fin_p$result)
