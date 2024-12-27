options(scipen=999)

input<-read.delim("~/data/advent/input7.data",sep="",header=F)
#input<-read.delim("~/data/advent/test",sep="",header=F)

input$V1=as.numeric(sub(pattern = ":",replacement = "",x = input$V1))
setDT(input)
source("~/data/advent/d7_functions.R")

# partie 1 ----------------------------------------------------------------
base=c("+","*")


row=vector()
for (nrow in 1:nrow(input)){
  res=F
  
  #1 nb_chiffres
  if (length(which(is.na(input[nrow,])))==0) fin=13 else fin=which(is.na(input[nrow,]))[1]-1
  nombres<-as.numeric(input[nrow,2:fin])
  
  #2 calculs
  comb<-create_combs(length(nombres),base)
  cible<-input[nrow,V1]
  for (k in 1:nrow(comb)){
    tmp<-calc(k ,nombres)
    #print(tmp)
    #3 test si calculs = V1
    if (tmp==cible){
      res=T
      break
      }
  }
  input[nrow,"test"]=res
  
}
input[,.N,test]
as.numeric(input[test==T,sum(V1)])->w
w
input$test=NULL

# partie 2 ----------------------------------------------------------------
base=c("+","*" ,"||")

for (nrow in 1:nrow(input)){
  res=F
  
  #1 nb_chiffres
  if (length(which(is.na(input[nrow,])))==0) fin=13 else fin=which(is.na(input[nrow,]))[1]-1
  nombres<-as.numeric(input[nrow,2:fin])
  
  #2 calculs
  comb<-create_combs(length(nombres),base)
  cible<-input[nrow,V1]
  for (k in 1:nrow(comb)){
    tmp<-calc(k ,nombres)
    if (tmp==cible){
      res=T
      break
    }
  }
  input[nrow,"test"]=res
}
input[,.N,test]
as.numeric(input[test==T,sum(V1)])->w
w
input$test=NULL

base=c("+","*","||")
create_combs(4,base)
