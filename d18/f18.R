calc_dist<-function(pos1,pos2){
  abs(pos2$x-pos1$x)+abs(pos2$y-pos1$y)
  
}
lister_options<-function(pos){
  px=pos$x
  py=pos$y
  opt<-data.table(matrix(ncol=2,nrow=0))
  colnames(opt)=c("dx","dy")
  if((px-1)>=0&nrow(obs[x==(px-1)&y==py,])==0){
    opt<-rbind(opt,data.table(-1,0),use.names=F)
  }
  if((px+1)<=xlim&nrow(obs[x==(px+1)&y==py,])==0){
    opt<-rbind(opt,data.table(1,0),use.names=F)
  }
  
  if((py+1)<=ylim&nrow(obs[x==px&y==(py+1),])==0){
    opt<-rbind(opt,data.table(0,1),use.names=F)
  }
  
  if((py-1)>=0&nrow(obs[x==px&y==(py-1),])==0){
    opt<-rbind(opt,data.table(0,-1),use.names=F)
  }

  return(opt)
}
calc_options<-function(pos,o){
  dx=o$dx
  dy=o$dy
  x=pos$x+dx
  y=pos$y+dy
  return(nxt=list(x=x,y=y))
}
construct_puzzle<-function(){
  df<-data.table(matrix(nrow=xlim+1,ncol=xlim+1))
  df[,]=as.character(rep(".",xlim+1))
  for (k in 1:nrow(obs)){
    df[obs[k,x]+1][[obs[k,y]+1]]="#"
    
  }
  return(df)
  
}
