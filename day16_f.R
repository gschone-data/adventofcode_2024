get_car<-function(data,car){
  w=which(data==car)
  x=w%%ncol(data)
  y=w%/%ncol(data)+1
  d=ifelse(car=="E",1,4)
  pos=list(x=x,y=y,d=d)
  return(pos)
}
calc_dist<-function(pos1,pos2){
  abs(pos2$x-pos1$x)+abs(pos2$y-pos1$y)
  
}






lister_options<-function(pos){
  x=pos$x
  y=pos$y
  d=pos$d
  opt<-data.table(matrix(ncol=3,nrow=0))
  colnames(opt)=c("dx","dy","d")
 if(d!=2&which(data[(x-1):1][[y]]=="#")[1]!=1){
    opt<-rbind(opt,data.table(-1,0,1),use.names=F)
 }
 if(d!=1&which(data[(x+1):nrow(data)][[y]]=="#")[1]!=1){
    opt<-rbind(opt,data.table(1,0,2),use.names=F)
  }
  if(d!=4&which(data[x,(y-1):1]=="#")[1]!=1){
    opt<-rbind(opt,data.table(0,-1,3),use.names=F)
  }
  
  if(d!=3&which(data[x,(y+1):ncol(data)]=="#")[1]!=1){
    opt<-rbind(opt,data.table(0,1,4),use.names=F)
  }
  
  #opt<-opt[!duplicated(opt[,.(dx,dy,d)])]
  return(opt)
}
calc_options<-function(pos,o){
  dx=o$dx
  dy=o$dy
  d=o$d
  x=pos$x+dx
  y=pos$y+dy
  return(nxt=list(x=x,y=y,d=d))
}
