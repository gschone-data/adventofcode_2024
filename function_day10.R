
inbound<-function(x,y){
  return(x<=nrow(data)&y<=nchar(data[x,]))
    }


next_k<-function(x,y,k){
    res=data.table(matrix(ncol=3,nrow=0))  
    colnames(res)=c("x","y","k")
    if (inbound(x,y+1)){
      if (as.integer(substr(data$V1[x],y+1,y+1))==k) res=rbind(res,data.table(x=x,y=y+1,k=k))
    }
    if ((y-1)!=0){if(inbound(x,y-1)){
      if (as.integer(substr(data$V1[x],y-1,y-1))==k) res=rbind(res,data.table(x=x,y=y-1,k=k))
    }}
    if ((x-1)!=0){if(inbound(x-1,y)){
      if (as.integer(substr(data$V1[x-1],y,y))==k) res=rbind(res,data.table(x=x-1,y=y,k=k))
    }}
    if (inbound(x+1,y)){
      if (as.integer(substr(data$V1[x+1],y,y))==k) res=rbind(res,data.table(x=x+1,y=y,k=k))
    }
    return(res)
}
