combo<-function(op){
  if (op %in% c(0:3)) res<-op
  if (op ==4) res<-A
  if (op==5) res<-B
  if (op==6) res<-C
  if (op==7) res<-NULL
return(res)
  }

traitement <-function(num,op){
  if (num==0){
    tmp <- as.integer(trunc(A/(2^combo(op))))
    A<<-tmp
    pointer<<-pointer+2
  }
  if(num==1){
    tmp<-bitwXor(B,op)
    B<<-tmp
    pointer<<-pointer+2
  }
  if(num==2){
    tmp<-combo(op)%%8
    B<<-tmp
    pointer<<-pointer+2
  }
  if(num==3){
    if(A!=0){
      pointer<<-op
    }else{
      pointer<<-pointer+2
    }}
  if(num==4){
    tmp<-bitwXor(B,C)
    B<<-tmp
    pointer<<-pointer+2
    }
  if(num==5){
    tmp<-combo(op)%%8
    tmp<-paste0(tmp,collapse = ",",",")
    out<<-paste0(out,tmp)
    pointer<<-pointer+2
  }
  if(num==6){
    tmp <- as.integer(trunc(A/(2^combo(op))))
    B<<-tmp
    pointer<-pointer+2
  }
  if(num==7){
    tmp <- as.integer(trunc(A/(2^combo(op))))
    C<<-tmp
    pointer<<-pointer+2
  }
return(tmp)
}

proc_lancement<-function(){
out<-""
pointer=0
while((pointer+1)<nrow(prog)){
  num=prog$V1[pointer+1]
  tmp<-traitement(num,prog$V1[pointer+2])  
  
}
    return(out)
} 
