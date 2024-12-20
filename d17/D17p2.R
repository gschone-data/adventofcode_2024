library(data.table)
data<-readLines("~/data/advent/input_17")
A<-as.integer(str_extract(data[1],"(?<=A: )\\d*"))
B<-as.integer(str_extract(data[2],"(?<=B: )\\d*"))
C<-as.integer(str_extract(data[3],"(?<=C: )\\d*"))
A0<-A
B0<-B
C0<-C
prog<-str_extract(data[5],"(?<=Program: ).*")
cible<-paste0(prog,",")
prog<-data.table(unlist(str_split(prog,",")))
prog$V1=as.integer(prog$V1)
rm(data)                  
A0=0
k=0
while(out!=cible){
A=A0+k
B=B0
C=C0
out=""
pointer=0
while((pointer+1)<nrow(prog)){
  num=prog$V1[pointer+1]
  tmp<-traitement(num,prog$V1[pointer+2])  
  
}
k=k+1
if(k%%100==0){print(k)}
}


#test
##cas1
C=9
traitement(2,6)
##cas2
A=10
prog=data.table(c(5,0,5,1,5,4))
##cas3
A=2024
prog=data.table(c(0,1,5,4,3,0))
proc_lancement()
##cas4 
B=29
traitement(1,7)
##cas5
B=2024
C=43690
traitement(4,0)
