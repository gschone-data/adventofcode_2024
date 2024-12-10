library(data.table)
library(gtools)
data<-read.delim("~/data/advent/input8",sep="",header=F)
source("~/data/advent/d8_functions.R")
data_w<-str_split(data$V1,"")
data_w<-as.data.frame(data_w)
data_w<-transpose(setDT(data_w))

type_antennes<-data_w[, unique(unlist(lapply(.SD, unique)))]

liste_points<-data.frame(matrix(ncol=3,nrow=0))
colnames(liste_points)=c("x","y","a")
for (type_a in type_antennes){
  if (type_a!="."){
    tmp<-data.frame(matrix(ncol=3,nrow=0))
    colnames(tmp)=c("x","y","a")
    for (row in 1:nrow(data_w)){
      test<-which(data_w[row,]==type_a)
      if (length(test)!=0)
      {
       x=rep(row,length(test))
       y=test
       a=type_a
        tmp<-rbind(tmp,data.frame(x,y,a))
      }
      
    }
    perm<-gtools::permutations(nrow(tmp),2,v=(1:nrow(tmp)),repeats.allowed = F)
    perm<-as.data.frame(perm)
    for(k in 1:nrow(perm)){
      antenne1<-tmp[perm[k,1],]
      antenne2<-tmp[perm[k,2],]
      points<-calcul_point(antenne1,antenne2)
      point1=list(x=points[[1]]$x,y=points[[1]]$y,a=type_a)
      point2=list(x=points[[2]]$x,y=points[[2]]$y,a=type_a)
      liste_points<-rbind(liste_points,point1,point2)
      }
  
    
  }
}

setDT(liste_points)
liste_points[x>=1&x<=50&y>=1&y<=50]->liste_ok
liste_ok |> group_by(x,y) |> summarise(n=n())->liste_ok_to
