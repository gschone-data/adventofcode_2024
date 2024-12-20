# data --------------------------------------------------------------------
data<-read.delim("day11.data",sep="",header=F,colClasses = "character")
data<-as.list(data)
data<-as.numeric(data)

#data<-as.numeric(c(125,17))

# fonction algo -----------------------------------------------------------

next_stones<-function(num){
  if (num==0){
    num=1
  }else{
    if (nchar(as.character(num))%%2==0){
      tmp_even=as.character(num)
      tmp_even1=as.numeric(substr(tmp_even,1,nchar(tmp_even)/2))
      tmp_even2=as.numeric(substr(tmp_even,nchar(tmp_even)/2+1,nchar(tmp_even)))
      num=c(tmp_even1,tmp_even2)
    }else{
      num=num*2024
    }}
  return(num)  
}


# boucle ------------------------------------------------------------------

tmp<-data
for(boucle in 26:75){
print(boucle)
j=1 
res=list()
for (k in 1:length(tmp)){
  stone=next_stones(tmp[k])
  res=append(res,stone,after=j)
  j=length(res)
}
tmp<-unlist(res)
}
head(tmp)
tail(tmp)
