get_start<-function(puzzle){
  w=which(puzzle=="@")
  y=w%%ncol(puzzle)
  x=w%/%ncol(puzzle)+1
  pos=list(x=x,y=y)
  return(pos)
  }

get_next<-function(pos,sens){
  dx<-case_when(sens=="^"~-1,
                sens=="v"~1,
                sens==">"~0,
                sens=="<"~0,
                T~5)
  dy<-case_when(sens=="^"~0,
                sens=="v"~0,
                sens==">"~1,
                sens=="<"~-1,
                T~5)
  
  x=pos$x+dx
  y=pos$y+dy
  pos=list(x=x,y=y)
  return(pos)
}

first_bloc<-function(pos,sens){
  x=pos$x
  y=pos$y
  vec<-case_when(
    sens=="^"~list(as.character(puzzle[(x-1):1,][[y]])),
    sens=="v"~list(as.character(puzzle[x+1:nrow(puzzle)][[y]])),
    sens=="<"~list(as.character(puzzle[x,(y-1):1])),
    sens==">"~list(as.character(puzzle[x,(y+1):ncol(puzzle)])))
  wall<-which(vec[[1]]=="#")[1]
  box<-ifelse(length(which(vec[[1]]=="O"))==0,-1,which(vec[[1]]=="O"))
  point<-ifelse(length(which(vec[[1]]=="."))==0,-1,which(vec[[1]]==".")[1])
  res=list(wall=wall,box=box,point=point)
  return(res)
  
}
deplacer_curs<-function(pos,sens){
  blocage=F
  x=pos$x
  y=pos$y
  nxt<-get_next(pos,sens)
  obs<-first_bloc(pos,sens)
  if (obs$point==-1|obs$point>obs$wall){blocage=T}
  res<-case_when(
    obs$wall==1~pos,
    obs$box==1&blocage==F~nxt,
    obs$point==1~nxt,
    T~pos)
  if(obs$box==1&blocage==F){bouge_box(pos,sens)}
  if(obs$point==1){bouge_curseur(pos,sens)}
  return(res)
}
recup_coord_obs<-function(pos,sens,curseur){
  x=pos$x
  y=pos$y
  obs<-case_when(sens=="^"~list(x=x-curseur,y=y),
            sens=="v"~list(x=x+curseur,y=y),
            sens==">"~list(x=x,y=y+curseur),
            sens=="<"~list(x=x,y=y-curseur))
  return(obs)
  
  
}

bouge_box<-function(pos,sens){
  t=T
  x=pos$x
  y=pos$y
  obs<-first_bloc(pos,sens)
  point<-recup_coord_obs(pos,sens,obs$point)
  prov<-puzzle
  prov[pos$x][[pos$y]]<-"."
  prov[point$x][[point$y]]<-"O"
  box<-recup_coord_obs(pos,sens,obs$box)
  prov[box$x][[box$y]]<-"@"
  puzzle<<-prov  
  
  return(t)
}
bouge_curseur<-function(pos,sens){
  t=T
  x=pos$x
  y=pos$y
  obs<-first_bloc(pos,sens)
  prov<-puzzle
  prov[pos$x][[pos$y]]<-"."
  point<-recup_coord_obs(pos,sens,obs$point)
  prov[point$x][[point$y]]<-"@"
  puzzle<<-prov
  return(t)
}
