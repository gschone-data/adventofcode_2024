read_data<-function(data_string){
  data_str<<-readLines(data_string)
  data<-str_split(data_str,"")
  data<-transpose(setDT(data))
  return(data)
}
nb_commun<-function(dt){
  tmp<-0
  x<-dt$x
  y<-dt$y
  if(inbound(x-1,y)==T){
    if (substr(data_str[x],y,y)==substr(data_str[x-1],y,y)) tmp<-tmp+1
  }
  if(inbound(x+1,y)==T){
    if (substr(data_str[x],y,y)==substr(data_str[x+1],y,y)) tmp<-tmp+1
  }
  if(inbound(x,y+1)==T){
    if (substr(data_str[x],y,y)==substr(data_str[x],y+1,y+1)) tmp<-tmp+1
  }
  if(inbound(x,y-1)==T){
    if (substr(data_str[x],y,y)==substr(data_str[x],y-1,y-1)) tmp<-tmp+1
  }
  return(tmp)
}


inbound<-function(x,y){
  return(x>=1&x<=nrow(data)&y>=1&y<=ncol(data))
}

dist<-function(p1,p2){
  return(abs(p1[1]$x-p2[1]$x)+abs(p1[1]$y-p2[1]$y))
}
dist_<-function(x1,y1,x2,y2){
  return(abs(x1-x2)+abs(y1-y2))
}
