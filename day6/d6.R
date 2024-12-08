library(data.table)
library(stringr)
library(dplyr)


source("~/avent_2024/day6/fonctions_d6.R")
data <-read.delim("input6_1.data",sep="",header=F)
data_w<-str_split(data$V1,"")

data_w<-as.data.frame(data_w)
data_w<-transpose(setDT(data_w))
pos0<-list(x=40,y=47,d=1)

liste_stop<-data.frame()
liste_stop<-rbind(liste_stop,pos0)
stop=pos0
repeat{
  stop<-next_stop(stop)
  liste_stop<-rbind(liste_stop,stop)
  print(stop)
  print(next_car(stop))
  if(test_pos(stop)==F){break}
  stop<-change_dir(stop)
  
}
liste_stop[135,"y"]=130

#liste_stop$ecart<-abs(liste_stop$x-lead(liste_stop$x))+abs(liste_stop$y-lead(liste_stop$y))
#sum(liste_stop$ecart,na.rm=T)


liste_positions<-data.frame(pos0)
for (k in 2:nrow(liste_stop)){
  
  pos1<-liste_stop[k-1,]
  pos2<-liste_stop[k,]
  temp<-make_liste_positions(pos1,pos2)
  liste_positions<-rbind(liste_positions,temp)
  
}
ggplot(liste_positions) +geom_point(aes(x=x,y=y),size=0.5)

##boucle_finale

liste_obs<-data.table()
data_w0<-data_w
nb_loop<-0
for (k in 2:nrow(liste_positions)){
  obs<-liste_positions[k,]
  data_w<-ajout_obstactle(obs)
  liste_stop<-data.table()
  liste_stop<-rbind(liste_stop,pos0)
  stop=pos0
  repeat{
    stop<-next_stop(stop)
    if(k!=2&nrow(liste_stop[x==stop$x&y==stop$y&d==stop$d,])>0){
      liste_obs<-rbind(liste_obs,obs)
      nb_loop<-nb_loop+1
      print(nb_loop)
      break
    }
    liste_stop<-rbind(liste_stop,stop)
    if(test_pos(stop)==F){break}
    stop<-change_dir(stop)
    if (next_car(stop)=="#"){change_dir(stop)}
    
    }
  data_w<-data_w0
}

liste_obs |> group_by(x,y) |> summarise(tot=n())->liste_obs_tot
liste_obs_tot |> filter(x==40)
stop
obs
saveRDS(liste_obs_tot,"resultP2.data")
data_w<-ajout_obstactle(liste_obs[500,])


py<- read.delim("~/avent_2024/day6/res_python.data",sep=",",header=F)
py$V2=sub(pattern = "\\)",replacement = "",x=py$V2)
py$V1=sub(pattern = "\\(",replacement = "",x=py$V1)
py$V1=as.integer(py$V1)
py$V2=as.integer(py$V2)
py$V3="py"
head(py)
liste_obs_tot |> left_join(py,by=join_by("x"=="V1","y"=="V2"))->test
setDT(test)
head(test)
test[is.na(V3)]
py |> group_by(V1,V2) |> summarise(n())->py_t

head(py_t)


for (k in 1:10){
  data_w<-ajout_obstactle(liste_obs_tot[k,])
  liste_stop<-data.table()
  liste_stop<-rbind(liste_stop,pos0)
  stop=pos0
  repeat{
    stop<-next_stop(stop)
    if(k!=2&nrow(liste_stop[x==stop$x&y==stop$y&d==stop$d,])>0){
      liste_obs<-rbind(liste_obs,obs)
      nb_loop<-nb_loop+1
      print(nb_loop)
      break
    }
    liste_stop<-rbind(liste_stop,stop)
    if(test_pos(stop)==F){break}
    stop<-change_dir(stop)
    if (next_car(stop)=="#"){change_dir(stop)}
    
  }
  
  liste_positions_tmp<-data.frame(pos0)
  for (k in 2:nrow(liste_stop)){
    
    pos1<-liste_stop[k-1,]
    pos2<-liste_stop[k,]
    temp<-make_liste_positions(pos1,pos2)
    liste_positions_tmp<-rbind(liste_positions_tmp,temp)
    
  }
  ggplot(liste_positions_tmp) +geom_point(aes(x=x,y=y),size=0.5)
  
  data_w<-data_w0
  
  
}
