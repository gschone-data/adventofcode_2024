pos<-list(x=40,y=47,d=1)
#class(pos)<-"pos"
pos

lire_pos<-function(pos){
  return(data_w[pos$x][[pos$y]])
}
# lire_pos<-function(x ,y ){
#     return(data_w[x,..y])
# }
change_dir<-function(pos){
  d=(pos$d+1)%%5+(pos$d+1)%/%5
  return(list(x=pos$x,y=pos$y,d=d))
}

next_stop<-function(pos){
  switch(pos$d,
    1=={
    tmp<-which(data_w[pos$x:1][[pos$y]]=="#")[1]
    pos_next<-list(x=pos$x-(tmp-2),y=pos$y,d=pos$d)
    },
  2=={
    tmp<-which(data_w[pos$x,pos$y:ncol(data_w)]=="#")[1]
    pos_next<-list(x=pos$x,y=pos$y+(tmp-2),d=pos$d)
  },
  3=={
    tmp<-which(data_w[pos$x:ncol(data_w)][[pos$y]]=="#")[1]
    pos_next<-list(x=pos$x+(tmp-2),y=pos$y,d=pos$d)
  },
  4=={
    tmp<-which(data_w[pos$x,pos$y:1]=="#")[1]
    
    pos_next<-list(x=pos$x,y=pos$y-(tmp-2),d=pos$d)
  }
  )
  return(pos_next)
}
 test_pos<-function(pos){
  length(pos$x)!=0&length(pos$y)!=0&!is.na(pos$x)&!is.na(pos$y)
}

next_car<-function(pos_){
 if (pos_$d==1){p1<-list(x=pos_$x-1,y=pos_$y,d=pos_$d)}
  if (pos_$d==2){p1<-list(x=pos_$x,y=pos_$y+1,d=pos_$d)}
  if (pos_$d==3){p1<-list(x=pos_$x+1,y=pos_$y,d=pos_$d)}
  if (pos_$d==4){p1<-list(x=pos_$x,y=pos_$y-1,d=pos_$d)}
  
    
  return(lire_pos(p1))
}

lire_autour<-function(pos){
return(data_w[(pos$x-2):(pos$x+2),(pos$y-2):(pos$y+2)])
}

ajout_obstactle<-function(pos){
  data_w[pos$x][[pos$y]]<-"#"
  return(data_w)
}

make_liste_positions<-function(pos1,pos2){
 if (pos1$x==pos2$x){
   liste_position = data.frame(
     x<-rep(pos1$x,length(pos1$y:pos2$y)),
     y<-pos1$y:pos2$y,
     d<-rep(pos2$d,length(pos1$y:pos2$y)))
 }
 if (pos1$y==pos2$y){
   liste_position = data.frame(
     x<-pos1$x:pos2$x,
     y<-rep(pos1$y,length(pos1$x:pos2$x)),
     d<-rep(pos2$d,length(pos1$x:pos2$x)))
 }
  colnames(liste_position)=c("x","y","d")
 return(liste_position[2:nrow(liste_position),])
}
   
   
   
   
   