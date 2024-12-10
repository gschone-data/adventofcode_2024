
antinodes<-function(p1,p2){
  res <-data.frame(matrix(ncol=2,nrow=0))
  res<-rbind(res,p1,p2)
  dx=p2$x-p1$x
  dy=p2$y-p1$y
  curX=p1$x
  curY=p1$y
  while (inbound(curX,curY)){
    curX =curX- dx
    curY =curY- dy
  if (!inbound(curX, curY)){
    break
  }
    p3=data.frame(x=curX,y=curY,a=p1$a)
    res=rbind(res,p3)
  }
  curX=p1$x
  curY=p1$y
  while (inbound(curX,curY)){
    curX =curX- dx
    curY =curY- dy
    if (!inbound(curX, curY)){
      break
    }
    p3=data.frame(x=curX,y=curY,a=p1$a)
    res=rbind(res,p3)
  }
 
  return(res) 
}
inbound<-function(a,b){
  test<-a>=1&a<=nrow(data_w)&b>=1&b<=nrow(data_w)
  return(test)  
 }  

# data --------------------------------------------------------------------

data<-read.delim("input8.data",sep="",header=F)
#data<-read.delim("test_8.data",sep="",header=F)
data_w<-str_split(data$V1,"")
data_w<-as.data.frame(data_w)
data_w<-transpose(setDT(data_w))

#liste antennes
liste_points<-data.frame(matrix(ncol=3,nrow=0))
colnames(liste_points)=c("x","y","a")
type_antennes<-data_w[, unique(unlist(lapply(.SD, unique)))]
liste_antennes<-data.frame(matrix(ncol=3,nrow=0))
colnames(liste_antennes)=c("x","y","a")
liste_droites<-data.frame(matrix(ncol=2,nrow=0))
colnames(liste_droites)=c("a","b")
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
    liste_antennes<-rbind(tmp,liste_antennes)
    perm<-gtools::permutations(nrow(tmp),2,v=(1:nrow(tmp)),repeats.allowed = F)
    perm<-as.data.frame(perm)
    for(k in 1:nrow(perm)){
      antenne1<-tmp[perm[k,1],]
      antenne2<-tmp[perm[k,2],]
      liste_tmp<-antinodes(antenne1,antenne2)
      liste_points<-rbind(liste_points,liste_tmp)
    }
    
    
  }
}
liste_points |> group_by(x,y) |> summarise(n=n()) -> liste_points_tot
